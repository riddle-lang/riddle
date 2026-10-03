use std::{collections::HashMap, hash::BuildHasher, sync::Arc};

use lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, Location, SymbolKind,
    TypeHierarchyItem,
};
use riddlec::pipeline::CompileOptions;
use serde_json::Value;
use tower_lsp::jsonrpc::Result;

use crate::{
    index::{
        IndexedSymbol, IndexedSymbolKind, ProjectIndex, SymbolKey,
        project_index_for_document_cancellable,
    },
    navigation::{definition_for_document_cancellable, type_definition_for_document_cancellable},
    server::Document,
    session::AnalysisSessions,
    workspace::WorkspaceState,
};

pub fn prepare_call_hierarchy<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: lsp_types::Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    workspace: &WorkspaceState,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<Vec<CallHierarchyItem>>, String> {
    let Some(index) = refresh_index(uri, docs, options, sessions, workspace, cancelled)? else {
        return Ok(None);
    };
    let Some(response) =
        definition_for_document_cancellable(uri, docs, position, options, sessions, cancelled)?
    else {
        return Ok(None);
    };
    let Some(location) = response_location(response) else {
        return Ok(None);
    };
    let Some(symbol) = symbol_at_location(&index, &location) else {
        return Ok(None);
    };
    if symbol.key.kind != IndexedSymbolKind::Function {
        return Ok(None);
    }
    Ok(Some(vec![call_item(symbol)]))
}

pub fn incoming_calls(
    item: &CallHierarchyItem,
    workspace: &WorkspaceState,
) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
    let key = decode_key(item.data.as_ref())?;
    let Some(index) = workspace.snapshot(&key.project) else {
        return Ok(Some(Vec::new()));
    };
    let calls = index
        .calls
        .iter()
        .filter(|edge| edge.target == key)
        .filter_map(|edge| {
            Some(CallHierarchyIncomingCall {
                from: call_item(index.symbol_by_key(&edge.caller)?),
                from_ranges: edge.sites.clone(),
            })
        })
        .collect();
    Ok(Some(calls))
}

pub fn outgoing_calls(
    item: &CallHierarchyItem,
    workspace: &WorkspaceState,
) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
    let key = decode_key(item.data.as_ref())?;
    let Some(index) = workspace.snapshot(&key.project) else {
        return Ok(Some(Vec::new()));
    };
    let calls = index
        .calls
        .iter()
        .filter(|edge| edge.caller == key)
        .filter_map(|edge| {
            Some(CallHierarchyOutgoingCall {
                to: call_item(index.symbol_by_key(&edge.target)?),
                from_ranges: edge.sites.clone(),
            })
        })
        .collect();
    Ok(Some(calls))
}

pub fn prepare_type_hierarchy<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    position: lsp_types::Position,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    workspace: &WorkspaceState,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<Vec<TypeHierarchyItem>>, String> {
    let Some(index) = refresh_index(uri, docs, options, sessions, workspace, cancelled)? else {
        return Ok(None);
    };
    let response = type_definition_for_document_cancellable(
        uri, docs, position, options, sessions, cancelled,
    )?
    .or(definition_for_document_cancellable(
        uri, docs, position, options, sessions, cancelled,
    )?);
    let Some(location) = response.and_then(response_location) else {
        return Ok(None);
    };
    let Some(symbol) = symbol_at_location(&index, &location) else {
        return Ok(None);
    };
    if type_symbol_kind(symbol.key.kind).is_none() {
        return Ok(None);
    }
    Ok(Some(vec![type_item(symbol)]))
}

pub fn supertypes(
    item: &TypeHierarchyItem,
    workspace: &WorkspaceState,
) -> Result<Option<Vec<TypeHierarchyItem>>> {
    related_types(item, workspace, true)
}

pub fn subtypes(
    item: &TypeHierarchyItem,
    workspace: &WorkspaceState,
) -> Result<Option<Vec<TypeHierarchyItem>>> {
    related_types(item, workspace, false)
}

fn related_types(
    item: &TypeHierarchyItem,
    workspace: &WorkspaceState,
    supertypes: bool,
) -> Result<Option<Vec<TypeHierarchyItem>>> {
    let key = decode_key(item.data.as_ref())?;
    let Some(index) = workspace.snapshot(&key.project) else {
        return Ok(Some(Vec::new()));
    };
    let keys = if supertypes {
        index.types.supertypes.get(&key)
    } else {
        index.types.subtypes.get(&key)
    };
    let items = keys
        .into_iter()
        .flatten()
        .filter_map(|key| index.symbol_by_key(key))
        .filter_map(|symbol| type_symbol_kind(symbol.key.kind).map(|_| type_item(symbol)))
        .collect();
    Ok(Some(items))
}

fn refresh_index<S: BuildHasher>(
    uri: &lsp_types::Url,
    docs: &HashMap<lsp_types::Url, Document, S>,
    options: CompileOptions,
    sessions: &AnalysisSessions,
    workspace: &WorkspaceState,
    cancelled: &impl Fn() -> bool,
) -> std::result::Result<Option<Arc<ProjectIndex>>, String> {
    let Some(index) =
        project_index_for_document_cancellable(uri, docs, options, sessions, cancelled)?
    else {
        return Ok(None);
    };
    if index.revision == 0 {
        return Ok(None);
    }
    let project = index.project.clone();
    let token = workspace.begin_rebuild(&project);
    if workspace.install(token, index) {
        Ok(workspace.snapshot(&project))
    } else {
        Ok(None)
    }
}

fn response_location(response: lsp_types::GotoDefinitionResponse) -> Option<Location> {
    match response {
        lsp_types::GotoDefinitionResponse::Scalar(location) => Some(location),
        lsp_types::GotoDefinitionResponse::Array(locations) => locations.into_iter().next(),
        lsp_types::GotoDefinitionResponse::Link(links) => links
            .into_iter()
            .next()
            .map(|link| Location::new(link.target_uri, link.target_selection_range)),
    }
}

fn symbol_at_location<'a>(
    index: &'a ProjectIndex,
    location: &Location,
) -> Option<&'a IndexedSymbol> {
    index.symbol_at(location)
}

fn call_item(symbol: &IndexedSymbol) -> CallHierarchyItem {
    CallHierarchyItem {
        name: symbol.name.clone(),
        kind: SymbolKind::FUNCTION,
        tags: None,
        detail: Some(symbol.detail.clone()),
        uri: symbol.location.uri.clone(),
        range: symbol.location.range,
        // `selectionRange` is the identifier the feature was invoked on, which
        // is what the client highlights; `range` is the whole item.
        selection_range: symbol.selection_range,
        data: Some(serde_json::to_value(&symbol.key).expect("symbol keys serialize")),
    }
}

fn type_item(symbol: &IndexedSymbol) -> TypeHierarchyItem {
    TypeHierarchyItem {
        name: symbol.name.clone(),
        kind: type_symbol_kind(symbol.key.kind).unwrap_or(SymbolKind::OBJECT),
        tags: None,
        detail: Some(symbol.detail.clone()),
        uri: symbol.location.uri.clone(),
        range: symbol.location.range,
        selection_range: symbol.selection_range,
        data: Some(serde_json::to_value(&symbol.key).expect("symbol keys serialize")),
    }
}

const fn type_symbol_kind(kind: IndexedSymbolKind) -> Option<SymbolKind> {
    match kind {
        IndexedSymbolKind::Struct => Some(SymbolKind::STRUCT),
        IndexedSymbolKind::Enum => Some(SymbolKind::ENUM),
        IndexedSymbolKind::Trait => Some(SymbolKind::INTERFACE),
        IndexedSymbolKind::TypeAlias => Some(SymbolKind::TYPE_PARAMETER),
        IndexedSymbolKind::Function
        | IndexedSymbolKind::Field
        | IndexedSymbolKind::EnumMember
        | IndexedSymbolKind::Const
        | IndexedSymbolKind::Module => None,
    }
}

fn decode_key(data: Option<&Value>) -> Result<SymbolKey> {
    let Some(data) = data else {
        return Err(tower_lsp::jsonrpc::Error::invalid_params(
            "hierarchy item has no symbol key",
        ));
    };
    serde_json::from_value(data.clone()).map_err(|_| {
        tower_lsp::jsonrpc::Error::invalid_params("hierarchy item has an invalid symbol key")
    })
}
