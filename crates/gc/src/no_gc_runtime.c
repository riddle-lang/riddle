#if !defined(_WIN32) && !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 199309L
#endif

#if defined(_MSC_VER) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static void riddle_memory_fail(void) {
    abort();
}

void *riddle_alloc(size_t size) {
    void *pointer = malloc(size ? size : 1u);
    if (!pointer) {
        riddle_memory_fail();
    }
    return pointer;
}

/* Untyped byte storage for standard-library code shared with the default
   runtime: there is no layout descriptor, so the payload is never scanned. */
void *riddle_alloc_bytes(size_t size) {
    return riddle_alloc(size);
}

void *riddle_realloc(void *pointer, size_t size) {
    void *next = realloc(pointer, size ? size : 1u);
    if (!next) {
        riddle_memory_fail();
    }
    return next;
}

void riddle_free(void *pointer) {
    free(pointer);
}

/* ---- std::fs shim (avoids clashing with <stdio.h> prototypes) ---- */
#include <stdio.h>
#include <stdint.h>
size_t riddle_fs_fopen(const char *path, const char *mode) {
    return (size_t)(uintptr_t)fopen(path, mode);
}
int riddle_fs_fclose(size_t stream) {
    return fclose((FILE *)(uintptr_t)stream);
}
size_t riddle_fs_fread(void *buffer, size_t size, size_t count, size_t stream) {
    return fread(buffer, size, count, (FILE *)(uintptr_t)stream);
}
size_t riddle_fs_fwrite(void *buffer, size_t size, size_t count, size_t stream) {
    return fwrite(buffer, size, count, (FILE *)(uintptr_t)stream);
}
int riddle_fs_fflush(size_t stream) {
    return fflush((FILE *)(uintptr_t)stream);
}

/* ---- std::time / std::random / std::fs (metadata + directory) shims ---- */
#include <time.h>
int64_t riddle_time(void *value) {
    return (int64_t)time((time_t *)value);
}

#if defined(_WIN32)
#include <windows.h>
void riddle_sleep_ms(uint64_t milliseconds) {
    Sleep((DWORD)milliseconds);
}
#else
void riddle_sleep_ms(uint64_t milliseconds) {
    struct timespec request;
    request.tv_sec = (time_t)(milliseconds / 1000u);
    request.tv_nsec = (long)((milliseconds % 1000u) * 1000000u);
    nanosleep(&request, NULL);
}
#endif

uint32_t riddle_random_u32(void) {
#if defined(_WIN32)
    static uint32_t state = 0;
    if (state == 0) {
        state = (uint32_t)GetTickCount() ^ (uint32_t)(uintptr_t)&state;
        if (state == 0) {
            state = 0x9e3779b9u;
        }
    }
    state ^= state << 13;
    state ^= state >> 17;
    state ^= state << 5;
    return state;
#else
    uint32_t value = 0;
    FILE *source = fopen("/dev/urandom", "rb");
    if (source != NULL) {
        size_t read = fread(&value, sizeof(value), 1, source);
        (void)read;
        fclose(source);
    }
    if (value == 0) {
        value = (uint32_t)time(0) ^ 0x9e3779b9u;
    }
    return value;
#endif
}

uint64_t riddle_random_u64(void) {
    uint64_t low = riddle_random_u32();
    uint64_t high = riddle_random_u32();
    return (high << 32) | low;
}

/* Keep the file-type tests available in strict POSIX feature-test modes. */
#include <sys/stat.h>
typedef struct stat RiddleStat;
#define RIDDLE_STAT(path, out) stat(path, out)
#if defined(_WIN32)
#define RIDDLE_IFDIR(mode) ((mode) & S_IFDIR)
#define RIDDLE_IFREG(mode) ((mode) & S_IFREG)
#else
#define RIDDLE_IFDIR(mode) S_ISDIR(mode)
#define RIDDLE_IFREG(mode) S_ISREG(mode)
#endif

int riddle_fs_exists(const char *path) {
    RiddleStat info;
    return RIDDLE_STAT(path, &info) == 0;
}

uint64_t riddle_fs_size(const char *path) {
    RiddleStat info;
    if (RIDDLE_STAT(path, &info) != 0) {
        return 0;
    }
    return (uint64_t)info.st_size;
}

int riddle_fs_is_file(const char *path) {
    RiddleStat info;
    if (RIDDLE_STAT(path, &info) != 0) {
        return 0;
    }
    return RIDDLE_IFREG(info.st_mode) != 0;
}

int riddle_fs_is_dir(const char *path) {
    RiddleStat info;
    if (RIDDLE_STAT(path, &info) != 0) {
        return 0;
    }
    return RIDDLE_IFDIR(info.st_mode) != 0;
}

size_t riddle_fs_read_dir(
    const char *path,
    void *names_buffer,
    void *lens_buffer,
    size_t capacity
) {
    const uint8_t **names_out = (const uint8_t **)names_buffer;
    size_t *lens_out = (size_t *)lens_buffer;
    size_t count = 0;
    size_t seen = 0;
#if defined(_WIN32)
    /* Use the wide API: FindFirstFileA returns names in the legacy ANSI
     * code page, so non-ASCII file names would be mangled or rejected by
     * the UTF-8 conversion in std. */
    WIN32_FIND_DATAW entry;
    wchar_t pattern_w[4096];
    {
        size_t path_len = strlen(path);
        int wide_len = MultiByteToWideChar(CP_UTF8, 0, path, (int)path_len, NULL, 0);
        if (wide_len <= 0 || (size_t)wide_len + 2 >= sizeof(pattern_w) / sizeof(pattern_w[0])) {
            return 0;
        }
        MultiByteToWideChar(CP_UTF8, 0, path, (int)path_len, pattern_w, wide_len);
        pattern_w[wide_len] = L'\\';
        pattern_w[wide_len + 1] = L'*';
        pattern_w[wide_len + 2] = L'\0';
    }
    HANDLE handle = FindFirstFileW(pattern_w, &entry);
    if (handle == INVALID_HANDLE_VALUE) {
        return 0;
    }
    do {
        const wchar_t *name = entry.cFileName;
        if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) {
            continue;
        }
        seen += 1;
        if (count >= capacity) {
            continue;
        }
        int utf8_len = WideCharToMultiByte(CP_UTF8, 0, name, -1, NULL, 0, NULL, NULL);
        if (utf8_len <= 0) {
            continue;
        }
        uint8_t *buffer = (uint8_t *)riddle_alloc((size_t)utf8_len);
        if (WideCharToMultiByte(CP_UTF8, 0, name, -1, (char *)buffer, utf8_len, NULL, NULL) == 0) {
            riddle_free(buffer);
            continue;
        }
        names_out[count] = buffer;
        lens_out[count] = (size_t)utf8_len - 1; /* exclude the NUL */
        count += 1;
    } while (FindNextFileW(handle, &entry));
    FindClose(handle);
#else
#include <dirent.h>
    DIR *dir = opendir(path);
    if (dir == NULL) {
        return 0;
    }
    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) {
            continue;
        }
        seen += 1;
        if (count >= capacity) {
            continue;
        }
        size_t len = strlen(name);
        uint8_t *buffer = (uint8_t *)riddle_alloc(len + 1);
        memcpy(buffer, name, len + 1);
        names_out[count] = buffer;
        lens_out[count] = len;
        count += 1;
    }
    closedir(dir);
#endif
    return seen > count ? seen : count;
}

/* ---- std::process / std::io / std::fs additions ---- */
void riddle_process_exit(int32_t code) {
    exit((int)code);
}

FILE *riddle_io_stderr(void) {
    return stderr;
}

size_t riddle_io_stdin(void) {
    return (size_t)(uintptr_t)stdin;
}

int riddle_fmt_fputc_stderr(int32_t value) {
    return fputc((int)value, stderr);
}

int riddle_fs_remove(const char *path) {
    return remove(path);
}

int riddle_fs_rename(const char *from, const char *to) {
    return rename(from, to);
}

void riddle_mem_swap(uint8_t *a, uint8_t *b, size_t size) {
    if (a == b || size == 0u) {
        return;
    }
    while (size >= 8u) {
        uint64_t temp8;
        memcpy(&temp8, a, 8u);
        memcpy(a, b, 8u);
        memcpy(b, &temp8, 8u);
        a += 8u;
        b += 8u;
        size -= 8u;
    }
    while (size > 0u) {
        uint8_t temp = *a;
        *a = *b;
        *b = temp;
        a += 1u;
        b += 1u;
        size -= 1u;
    }
}

int32_t riddle_fs_fgetc(size_t stream) {
    return (int32_t)fgetc((FILE *)(uintptr_t)stream);
}
