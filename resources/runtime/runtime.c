#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Mark-and-Sweep

typedef struct GCObject {
    size_t size;
    int marked;
    struct GCObject* next;
    // Data
} GCObject;

static GCObject* g_gc_objects = NULL;
static void* g_stack_bottom = NULL;
static size_t g_allocated_bytes = 0;
static size_t g_gc_threshold = 1024 * 1024; // 1MB

void riddle_gc_init() {
    int stack_var;
    g_stack_bottom = &stack_var;
    // printf("GC: initialized, stack bottom around %p\n", g_stack_bottom);
}

void mark_object(void* ptr) {
    if (!ptr) return;

    // Check whether the pointer is in our object list
    GCObject* curr = g_gc_objects;
    while (curr) {
        void* obj_data = (void*)(curr + 1);
        if (ptr == obj_data) {
            if (curr->marked) return;
            curr->marked = 1;
            
            // Conservatively scan for pointers that may exist within the object
            // Assume all 8-byte aligned bit patterns within the object could potentially be pointers
            uintptr_t* data = (uintptr_t*)obj_data;
            size_t words = curr->size / sizeof(uintptr_t);
            for (size_t i = 0; i < words; i++) {
                mark_object((void*)data[i]);
            }
            return;
        }
        curr = curr->next;
    }
}

void riddle_gc_collect() {
    // Reset all flags
    GCObject* curr = g_gc_objects;
    while (curr) {
        curr->marked = 0;
        curr = curr->next;
    }

    // Scan Stack (Root)
    int stack_top_var;
    void* stack_top = &stack_top_var;
    
    void** start = (void**)stack_top;
    void** end = (void**)g_stack_bottom;
    
    // Low-to-high scanning
    if (start > end) {
        void** tmp = start;
        start = end;
        end = tmp;
    }

    // printf("GC: scanning stack from %p to %p\n", start, end);
    for (void** p = start; p < end; p++) {
        mark_object(*p);
    }

    // Remove unmarked objects
    GCObject** pp = &g_gc_objects;
    size_t freed_count = 0;
    while (*pp) {
        GCObject* obj = *pp;
        if (!obj->marked) {
            *pp = obj->next;
            g_allocated_bytes -= obj->size;
            // printf("GC: freeing %p (size %zu)\n", (void*)(obj + 1), obj->size);
            free(obj);
            freed_count++;
        } else {
            pp = &obj->next;
        }
    }
    
    // printf("GC: collection finished, freed %zu objects\n", freed_count);
    
    // Adjust the threshold
    g_gc_threshold = g_allocated_bytes * 2;
    if (g_gc_threshold < 1024 * 1024) g_gc_threshold = 1024 * 1024;
}

void* riddle_gc_alloc(size_t size) {
    if (g_allocated_bytes + size > g_gc_threshold) {
        riddle_gc_collect();
    }

    // Align to 8 bytes
    size = (size + 7) & ~7;

    GCObject* obj = (GCObject*)malloc(sizeof(GCObject) + size);
    if (!obj) {
        riddle_gc_collect(); // 尝试回收后再分配
        obj = (GCObject*)malloc(sizeof(GCObject) + size);
        if (!obj) {
            fprintf(stderr, "Out of memory\n");
            exit(1);
        }
    }

    obj->size = size;
    obj->marked = 0;
    obj->next = g_gc_objects;
    g_gc_objects = obj;
    g_allocated_bytes += size;

    void* ptr = (void*)(obj + 1);
    memset(ptr, 0, size);
    // printf("GC: allocated %zu bytes at %p\n", size, ptr);
    return ptr;
}

struct RiddleVec {
    void* data;
    int64_t len;
    int64_t cap;
    int64_t elem_size;
};

struct RiddleVec* riddle_vec_reserve(void* data, int64_t len, int64_t cap, int64_t elem_size) {
    // printf("DEBUG: reserve called, data=%p, len=%lld, cap=%lld, size=%lld\n", data, (long long)len, (long long)cap, (long long)elem_size);
    if (len < cap) {
        struct RiddleVec* out = riddle_gc_alloc(sizeof(struct RiddleVec));
        out->data = data;
        out->len = len;
        out->cap = cap;
        out->elem_size = elem_size;
        return out;
    }
    int64_t new_cap = cap == 0 ? 4 : cap * 2;
    // printf("DEBUG: expanding to %lld\n", (long long)new_cap);
    void* new_data = riddle_gc_alloc(new_cap * elem_size);
    if (cap > 0) {
        memcpy(new_data, data, len * elem_size);
    }
    struct RiddleVec* out = riddle_gc_alloc(sizeof(struct RiddleVec));
    out->data = new_data;
    out->len = len;
    out->cap = new_cap;
    out->elem_size = elem_size;
    return out;
}
