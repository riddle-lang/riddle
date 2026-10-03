use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicU64, Ordering},
    },
};

use riddlec::pipeline::CheckSession;

use crate::server::Document;

/// Logs when an analysis session is created, under `RIDDLEC_PHASE_TIMING`.
///
/// A `CheckSession` or `ProjectSession` that is created more than once for the
/// same document or project means the incremental state behind it — including
/// the standard library's ~827 checked bodies — was thrown away.
fn trace_session(event: &str, key: &str) {
    if std::env::var_os("RIDDLEC_PHASE_TIMING").is_some() {
        let name = key.rsplit(['/', '\\']).next().unwrap_or(key);
        eprintln!("[session] {event:<30} {name}");
    }
}

#[derive(Default)]
pub struct AnalysisSessions {
    standalone: Mutex<HashMap<lsp_types::Url, Arc<Mutex<CheckSession>>>>,
    projects: Mutex<HashMap<PathBuf, Arc<Mutex<clue::ProjectSession>>>>,
    /// Process-unique identity, so a trace can tell which of the server's
    /// several session sets a request landed on. The server builds one set for
    /// analysis and one for completion, and only a trace can show which one a
    /// given request used.
    id: AtomicU64,
}

impl AnalysisSessions {
    /// Assigns the process-unique identity on first use.
    fn identity(&self) -> u64 {
        if self.id.load(Ordering::Relaxed) == 0 {
            static NEXT_ID: AtomicU64 = AtomicU64::new(1);
            self.id
                .store(NEXT_ID.fetch_add(1, Ordering::Relaxed), Ordering::Relaxed);
        }
        self.id.load(Ordering::Relaxed)
    }

    pub(crate) fn standalone(&self, uri: &lsp_types::Url) -> Arc<Mutex<CheckSession>> {
        let mut standalone = self.standalone.lock().unwrap();
        if !standalone.contains_key(uri) {
            trace_session(
                &format!("analysis standalone: create (set {})", self.identity()),
                uri.as_str(),
            );
        }
        Arc::clone(standalone.entry(uri.clone()).or_default())
    }

    pub(crate) fn project(&self, root: &std::path::Path) -> Arc<Mutex<clue::ProjectSession>> {
        let mut projects = self.projects.lock().unwrap();
        if !projects.contains_key(root) {
            trace_session(
                &format!("analysis project: create (set {})", self.identity()),
                &root.display().to_string(),
            );
        }
        Arc::clone(projects.entry(root.to_path_buf()).or_default())
    }

    pub(crate) fn retain_open(&self, docs: &HashMap<lsp_types::Url, Document>) {
        self.standalone
            .lock()
            .unwrap()
            .retain(|uri, _| docs.contains_key(uri));
        let roots: HashSet<_> = docs
            .keys()
            .filter_map(|uri| uri.to_file_path().ok())
            .filter_map(|path| clue::find_project_root(&path))
            .collect();
        self.projects
            .lock()
            .unwrap()
            .retain(|root, _| roots.contains(root));
    }

    pub(crate) fn clear_projects(&self) {
        self.projects.lock().unwrap().clear();
    }

    pub(crate) fn invalidate_roots<'a>(&self, roots: impl IntoIterator<Item = &'a PathBuf>) {
        let mut projects = self.projects.lock().unwrap();
        for root in roots {
            projects.remove(root);
        }
    }

    pub(crate) fn invalidate_project(&self, uri: &lsp_types::Url) {
        let Some(root) = uri
            .to_file_path()
            .ok()
            .and_then(|path| clue::find_project_root(&path))
        else {
            return;
        };
        self.projects.lock().unwrap().remove(&root);
    }

    pub(crate) fn current_revision(
        &self,
        uri: &lsp_types::Url,
        docs: &HashMap<lsp_types::Url, Document>,
    ) -> Option<u64> {
        let Ok(path) = uri.to_file_path() else {
            return Some(0);
        };
        let Some(root) = clue::find_project_root(&path) else {
            return Some(0);
        };
        let overlays = docs
            .iter()
            .filter_map(|(uri, document)| {
                uri.to_file_path()
                    .ok()
                    .map(|path| (path, document.text.clone()))
            })
            .collect::<HashMap<_, _>>();
        let session = self.projects.lock().unwrap().get(&root).cloned()?;
        let session = session
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        session
            .inputs_are_current(&overlays)
            .then(|| session.revision())
    }

    pub(crate) fn revision(&self, uri: &lsp_types::Url) -> u64 {
        let Some(root) = uri
            .to_file_path()
            .ok()
            .and_then(|path| clue::find_project_root(&path))
        else {
            return 0;
        };
        self.projects
            .lock()
            .unwrap()
            .get(&root)
            .map_or(0, |session| {
                session
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .revision()
            })
    }
}
