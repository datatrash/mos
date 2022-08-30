use crate::codegen::{Symbol, SymbolType};
use crate::parser::{Identifier, IdentifierPath};
use itertools::Itertools;
use petgraph::graph::NodeIndex;
use petgraph::prelude::EdgeRef;
use petgraph::stable_graph::{NodeIndices, StableGraph};
use petgraph::Direction;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

#[derive(Clone)]
pub struct SymbolTable<S: Clone + Debug> {
    graph: StableGraph<Item<S>, Identifier>,
    pub root: SymbolIndex,
}

pub type SymbolIndex = NodeIndex;
pub type SymbolIndices<'a, S> = NodeIndices<'a, Item<S>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryTraversalStep {
    Symbol(SymbolIndex),
    Super(SymbolIndex),
}

#[derive(Clone, Debug)]
pub struct Item<S: Clone + Debug> {
    data: Option<S>,
}

impl<S: Clone + Debug> Item<S> {
    fn new<OS: Into<Option<S>>>(data: OS) -> Self {
        Self { data: data.into() }
    }
}

impl<S: Clone + Debug> Default for SymbolTable<S> {
    fn default() -> Self {
        let mut graph = StableGraph::new();
        let root = graph.add_node(Item::new(None));

        Self { graph, root }
    }
}

impl<S: Clone + Debug> SymbolTable<S> {
    pub fn indices(&self) -> SymbolIndices<S> {
        self.graph.node_indices()
    }

    pub fn get(&self, nx: SymbolIndex) -> &Option<S> {
        &self.graph[nx].data
    }

    pub fn try_get(&self, nx: SymbolIndex) -> Option<&S> {
        self.graph.node_weight(nx).and_then(|i| i.data.as_ref())
    }

    pub fn try_get_mut(&mut self, nx: SymbolIndex) -> Option<&mut S> {
        self.graph.node_weight_mut(nx).and_then(|i| i.data.as_mut())
    }

    pub fn insert<I: Into<Identifier>, D: Into<Option<S>>>(
        &mut self,
        parent_nx: SymbolIndex,
        id: I,
        data: D,
    ) -> SymbolIndex {
        let id = id.into();
        let new_nx = self.graph.add_node(Item { data: data.into() });
        log::trace!(
            "Inserted node '{}' ({:?}) (parent: {:?})",
            &id,
            new_nx,
            parent_nx
        );
        self.graph.add_edge(parent_nx, new_nx, id);
        new_nx
    }

    pub fn update_data<D: Into<Option<S>>>(&mut self, nx: SymbolIndex, data: D) {
        self.graph[nx].data = data.into();
    }

    pub fn export<I: Into<IdentifierPath>>(
        &mut self,
        to_export_nx: SymbolIndex,
        new_parent_nx: SymbolIndex,
        new_path: I,
    ) -> bool {
        let new_path = new_path.into();
        let (new_path, new_id) = new_path.split();

        // Make sure the path is already present
        let new_nx = self.ensure_index(new_parent_nx, new_path);

        // Check if there's already an edge called 'new_id' that goes somewhere different than 'to_export_nx'
        if self
            .graph
            .edges_directed(new_nx, Direction::Outgoing)
            .any(|edge| edge.target() != to_export_nx && edge.weight() == &new_id)
        {
            false
        } else {
            // Doesn't exist yet. So, now link 'new_nx' to 'to_export_nx' via 'new_id'
            log::trace!(
                "Exporting '{:?}' to '{:?}' via '{}'",
                new_nx,
                to_export_nx,
                new_id
            );
            self.graph.add_edge(new_nx, to_export_nx, new_id);
            true
        }
    }

    pub fn remove(&mut self, nx: SymbolIndex) {
        log::trace!("Removing node: {:?}", nx);
        self.graph.remove_node(nx);
    }

    pub fn remove_all(&mut self, nx: SymbolIndex) {
        for child in self.children(nx).values() {
            self.remove_all(*child);
        }
        self.remove(nx);
    }

    pub fn ensure_index<I: Into<IdentifierPath>>(
        &mut self,
        mut nx: SymbolIndex,
        path: I,
    ) -> SymbolIndex {
        let mut path = path.into();
        while !path.is_empty() {
            let id = path.pop_front().unwrap();
            nx = match self.child(nx, &id) {
                Some(nx) => nx,
                None => self.insert(nx, id, None),
            };
        }
        nx
    }

    pub fn index<I: Into<IdentifierPath>>(&self, nx: SymbolIndex, path: I) -> SymbolIndex {
        self.try_index(nx, path).unwrap()
    }

    pub fn try_index<I: Into<IdentifierPath>>(
        &self,
        nx: SymbolIndex,
        path: I,
    ) -> Option<SymbolIndex> {
        let mut path = path.into();

        let mut cur_nx = Some(nx);
        while !path.is_empty() && cur_nx.is_some() {
            let nx = cur_nx.unwrap();
            let id = path.pop_front().unwrap();
            cur_nx = if id.is_super() {
                self.parent(nx)
            } else {
                self.child(nx, &id)
            };
        }

        assert!(path.is_empty() || cur_nx.is_none());

        cur_nx
    }

    pub fn parent(&self, nx: SymbolIndex) -> Option<SymbolIndex> {
        let mut edges = self.graph.edges_directed(nx, Direction::Incoming);
        edges.next().map(|edge| edge.source())
    }

    pub fn child(&self, nx: SymbolIndex, id: &Identifier) -> Option<SymbolIndex> {
        for child in self.graph.edges_directed(nx, Direction::Outgoing) {
            if child.weight() == id {
                return Some(child.target());
            }
        }
        None
    }

    pub fn children(&self, nx: SymbolIndex) -> HashMap<Identifier, SymbolIndex> {
        self.graph
            .edges_directed(nx, Direction::Outgoing)
            .map(|edge| (edge.weight().clone(), edge.target()))
            .collect()
    }

    pub fn rename(&mut self, nx: SymbolIndex, child_nx: SymbolIndex, new_id: Identifier) {
        let edge_ids = self
            .graph
            .edges_directed(nx, Direction::Outgoing)
            .filter(|edge| edge.target() == child_nx)
            .map(|edge| {
                log::trace!(
                    "About to rename symbol (edge from {:?} -> {:?}) from '{}' to '{}'",
                    edge.source(),
                    edge.target(),
                    edge.weight(),
                    new_id
                );
                edge.id()
            })
            .collect_vec();

        for id in edge_ids {
            if let Some(weight) = self.graph.edge_weight_mut(id) {
                *weight = new_id.clone();
            }
        }
    }

    pub fn query<I: Into<IdentifierPath>>(&self, nx: SymbolIndex, path: I) -> Option<SymbolIndex> {
        self.query_traversal_steps(nx, path)
            .last()
            .and_then(|tx| match tx {
                QueryTraversalStep::Symbol(nx) => Some(*nx),
                _ => None,
            })
    }

    /// Query for the symbol on every level, bubbling up
    /// The difference with [query] is that this doesn't just return the nearest symbol in scope
    pub fn query_all<I: Into<IdentifierPath>>(&self, nx: SymbolIndex, path: I) -> Vec<SymbolIndex> {
        let path = path.into();
        let mut result = vec![];

        let mut cur_nx = Some(nx);
        while cur_nx.is_some() {
            if let Some(query_nx) = self.query(cur_nx.unwrap(), path.clone()) {
                result.push(query_nx);
            }
            cur_nx = self.parent(cur_nx.unwrap());
        }

        result
    }

    pub fn query_traversal_steps<I: Into<IdentifierPath>>(
        &self,
        nx: SymbolIndex,
        path: I,
    ) -> Vec<QueryTraversalStep> {
        let path = path.into();
        log::trace!(
            "Getting query traversal steps for path '{}' with parent '{:?}'",
            &path,
            nx
        );
        let mut ids = path.clone();
        let mut cur_nx = Some(nx);
        let mut traversal = vec![];
        while let Some(id) = ids.pop_front() {
            log::trace!("`--> Looking up id '{}' in parent: '{:?}'", id, cur_nx);
            if let Some(c) = cur_nx {
                traversal.push(QueryTraversalStep::Symbol(c));
                cur_nx = self.try_index(c, id);
                log::trace!("   `--> This resulted in: {:?}", cur_nx);
            }
        }

        // Remove the first element since that was our starting point
        if let Some((_, rest)) = traversal.split_first() {
            traversal = rest.to_vec();
        }

        match cur_nx {
            Some(nx) => {
                traversal.push(QueryTraversalStep::Symbol(nx));
                traversal
            }
            None if path.contains_super() => vec![],
            None => {
                // If path is not explicitly referring to 'super', we can bubble up and try again
                match self.parent(nx) {
                    Some(parent_nx) => {
                        let mut traversal = vec![QueryTraversalStep::Super(parent_nx)];
                        traversal.extend(self.query_traversal_steps(parent_nx, path));
                        traversal
                    }
                    None => {
                        // We cannot bubble up anymore, but didn't complete our full traversal.
                        // So...symbol not found, unfortunately
                        vec![]
                    }
                }
            }
        }
    }

    pub fn query_steps_to_path(
        &self,
        mut nx: SymbolIndex,
        steps: &[QueryTraversalStep],
        include_super: bool,
    ) -> Option<IdentifierPath> {
        let mut ids = vec![];
        for step in steps {
            match step {
                QueryTraversalStep::Symbol(child_nx) => {
                    let edge = self
                        .graph
                        .edges_directed(nx, Direction::Outgoing)
                        .find(|edge| edge.target() == *child_nx);

                    match edge {
                        Some(edge) => {
                            ids.push(edge.weight().clone());
                            nx = edge.target();
                        }
                        None => {
                            // Bail entirely, because this query traversal isn't valid
                            return None;
                        }
                    }
                }
                QueryTraversalStep::Super(parent_nx) => {
                    if include_super {
                        ids.push(Identifier::sup());
                    }
                    nx = *parent_nx;
                }
            }
        }
        Some(IdentifierPath::new(&ids))
    }

    pub fn visible_symbols(
        &self,
        nx: SymbolIndex,
        bubble_up: bool,
    ) -> HashMap<IdentifierPath, SymbolIndex> {
        let mut result = HashMap::new();
        let mut added_to_result = HashSet::new();
        let mut visited = HashSet::new();

        let mut nx = Some(nx);
        let mut path = IdentifierPath::empty();
        while nx.is_some() {
            visited.insert(nx.unwrap());

            for edge in self.graph.edges_directed(nx.unwrap(), Direction::Outgoing) {
                if visited.contains(&edge.target()) {
                    continue;
                }

                if added_to_result.contains(&edge.target()) {
                    //continue;
                }
                added_to_result.insert(edge.target());

                let id = edge.weight();
                let full_path = path.join(id);
                match result.entry(IdentifierPath::from(id)) {
                    Entry::Occupied(_) => {
                        result.insert(full_path, edge.target());
                    }
                    Entry::Vacant(e) => {
                        e.insert(edge.target());
                    }
                }
            }

            if !bubble_up {
                break;
            }

            nx = self.parent(nx.unwrap());
            path = path.join(Identifier::sup());
        }

        result
    }

    pub fn all(&self) -> HashMap<IdentifierPath, (SymbolIndex, &S)> {
        let mut result = HashMap::new();
        self.all_impl(&mut result, self.root, "".into());
        result
    }

    fn all_impl<'a>(
        &'a self,
        map: &mut HashMap<IdentifierPath, (SymbolIndex, &'a S)>,
        nx: SymbolIndex,
        path: IdentifierPath,
    ) {
        if let Some(data) = self.try_get(nx) {
            map.insert(path.clone(), (nx, data));
        }
        for (child_id, child_nx) in self.children(nx) {
            self.all_impl(map, child_nx, path.join(child_id));
        }
    }
}

impl SymbolTable<Symbol> {
    pub fn ensure_cpu_symbols(&mut self, registers: HashMap<String, i64>, flags: u8) {
        let root = self.root;
        let cpu_nx = self.ensure_index(root, "cpu");
        let cpu_flags_nx = self.ensure_index(cpu_nx, "flags");

        let mut add = |parent_nx: SymbolIndex, id: &str, data: Option<&i64>, ty: SymbolType| {
            if let Some(data) = data {
                let symbol = Symbol {
                    pass_idx: 0,
                    segment: None,
                    span: None,
                    data: (*data).into(),
                    ty,
                };

                if let Some(symbol_nx) = self.try_index(parent_nx, id) {
                    self.update_data(symbol_nx, symbol);
                } else {
                    self.insert(parent_nx, id, symbol);
                }
            }
        };

        add(cpu_nx, "sp", registers.get("SP"), SymbolType::Label);
        add(cpu_nx, "a", registers.get("A"), SymbolType::Constant);
        add(cpu_nx, "x", registers.get("X"), SymbolType::Constant);
        add(cpu_nx, "y", registers.get("Y"), SymbolType::Constant);

        add(
            cpu_flags_nx,
            "carry",
            Some(&((flags & 1) as i64)),
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "zero",
            Some(&((flags & 2) as i64)),
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "interrupt_disable",
            Some(&((flags & 4) as i64)),
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "decimal",
            Some(&((flags & 8) as i64)),
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "overflow",
            Some(&((flags & 64) as i64)),
            SymbolType::Constant,
        );
        add(
            cpu_flags_nx,
            "negative",
            Some(&((flags & 128) as i64)),
            SymbolType::Constant,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{id, idpath};
    use itertools::Itertools;
    use mos_testing::assert_unordered_eq;

    #[test]
    fn navigation() {
        let t = table();

        let s_ss_a = t.table.index(t.table.root, "S.SS.a");
        assert_eq!(s_ss_a, t.s_ss_a);

        assert_eq!(t.table.parent(t.s_ss_a), Some(t.s_ss));
        assert_eq!(t.table.child(t.s_ss, &id!("a")), Some(t.s_ss_a));

        let mut children = t
            .table
            .children(t.t)
            .into_iter()
            .map(|(_, nx)| nx)
            .collect_vec();
        children.sort();
        assert_eq!(children, &[t.t_a, t.t_b, t.t_u]);
    }

    #[test]
    fn query() {
        let mut t = table();

        // Can query symbols that are on the same level
        assert_eq!(t.table.query(t.s_ss, &id!("a")), Some(t.s_ss_a));
        assert_eq!(t.table.query(t.s, &id!("a")), Some(t.s_a));
        assert_eq!(t.table.query(t.table.root, &id!("a")), Some(t.a));

        // Can query symbols using 'super'
        assert_eq!(t.table.query(t.s_ss, &idpath!("super.a")), Some(t.s_a));
        assert_eq!(t.table.query(t.s, &idpath!("super.a")), Some(t.a));
        assert_eq!(t.table.query(t.table.root, &idpath!("super.a")), None);

        // Can query symbols that have a longer path, on a higher level (requiring bubbling up)
        assert_eq!(t.table.query(t.s_ss, &idpath!("T.U.a")), Some(t.t_u_a));

        // Can also query symbols that exist on a higher level
        let nx_c = t.table.insert(t.table.root, "c", 50);
        assert_eq!(t.table.query(t.s_ss, &id!("c")), Some(nx_c));

        // Cannot query symbols that do not exist
        assert_eq!(t.table.query(t.s, &id!("blah")), None);
        assert_eq!(t.table.query(t.table.root, &idpath!("S.SS.blah")), None);
    }

    #[test]
    fn query_all() {
        let t = table();
        assert_eq!(
            t.table.query_all(t.s_ss, &id!("a")),
            vec![t.s_ss_a, t.s_a, t.a]
        );
    }

    #[test]
    fn query_traversal_steps() {
        let t = table();
        let steps = t.table.query_traversal_steps(t.s_ss, &idpath!("T.U.a"));
        assert_eq!(
            steps,
            vec![
                QueryTraversalStep::Super(t.s),
                QueryTraversalStep::Super(t.table.root),
                QueryTraversalStep::Symbol(t.t),
                QueryTraversalStep::Symbol(t.t_u),
                QueryTraversalStep::Symbol(t.t_u_a),
            ]
        );

        assert_eq!(
            t.table.query_steps_to_path(t.s_ss, &steps, false),
            Some(idpath!("T.U.a"))
        );
        assert_eq!(
            t.table.query_steps_to_path(t.s_ss, &steps, true),
            Some(idpath!("super.super.T.U.a"))
        );
    }

    #[test]
    fn visible_symbols() {
        let t = table();
        let vis = t
            .table
            .visible_symbols(t.t, true)
            .into_iter()
            .map(|(id, _)| id)
            .collect_vec();
        assert_unordered_eq(
            &vis,
            &[
                idpath!("a"),
                idpath!("b"),
                idpath!("U"),
                idpath!("super.a"),
                idpath!("super.b"),
                idpath!("S"),
            ],
        );

        let vis = t
            .table
            .visible_symbols(t.t, false)
            .into_iter()
            .map(|(id, _)| id)
            .collect_vec();
        assert_unordered_eq(&vis, &[idpath!("a"), idpath!("b"), idpath!("U")]);

        let vis = t
            .table
            .visible_symbols(t.t, true)
            .into_iter()
            .map(|(_, idx)| idx)
            .collect_vec();
        assert_unordered_eq(
            &vis,
            &[
                SymbolIndex::new(1),
                SymbolIndex::new(2),
                SymbolIndex::new(3),
                SymbolIndex::new(7),
                SymbolIndex::new(8),
                SymbolIndex::new(9),
            ],
        );
    }

    #[test]
    fn ensure_index() {
        let mut t = SymbolTable::default();
        let nx = t.ensure_index(t.root, "a.b.c");
        t.insert(nx, "d", 123);
        assert_eq!(t.try_get(t.index(t.root, "a.b.c.d")), Some(&123));
    }

    #[test]
    fn remove() {
        let mut t = table();
        t.table.remove(t.s);
        assert_eq!(t.table.try_get(t.s), None);
        assert_eq!(t.table.parent(t.s_ss_a), Some(t.s_ss));
    }

    #[test]
    fn remove_all() {
        let mut t = table();
        t.table.remove_all(t.s);
        assert_eq!(t.table.try_get(t.s), None);
        assert_eq!(t.table.parent(t.s_ss_a), None);
    }

    #[test]
    fn export() {
        let mut t = table();

        // S.SS.a will be exported as 'T.foo'
        t.table.export(t.s_ss_a, t.t, "foo");
        assert_eq!(t.table.index(t.table.root, "T.foo"), t.s_ss_a);
    }

    #[allow(dead_code)]
    struct TestTable {
        table: SymbolTable<i32>,
        a: SymbolIndex,
        b: SymbolIndex,
        s: NodeIndex<u32>,
        s_a: NodeIndex<u32>,
        s_b: NodeIndex<u32>,
        t: NodeIndex<u32>,
        t_a: NodeIndex<u32>,
        t_b: NodeIndex<u32>,
        t_u: NodeIndex<u32>,
        t_u_a: NodeIndex<u32>,
        t_u_b: NodeIndex<u32>,
        s_ss: NodeIndex<u32>,
        s_ss_a: NodeIndex<u32>,
        s_ss_b: NodeIndex<u32>,
    }

    fn table() -> TestTable {
        let mut table = SymbolTable::default();
        let a = table.insert(table.root, "a", 1);
        let b = table.insert(table.root, "b", 2);
        let s = table.insert(table.root, "S", None);
        let s_a = table.insert(s, "a", 3);
        let s_b = table.insert(s, "b", 4);
        let t = table.insert(table.root, "T", None);
        let t_a = table.insert(t, "a", 5);
        let t_b = table.insert(t, "b", 6);
        let t_u = table.insert(t, "U", None);
        let t_u_a = table.insert(t_u, "a", 7);
        let t_u_b = table.insert(t_u, "b", 8);
        let s_ss = table.insert(s, "SS", None);
        let s_ss_a = table.insert(s_ss, "a", 9);
        let s_ss_b = table.insert(s_ss, "b", 10);
        TestTable {
            table,
            a,
            b,
            s,
            s_a,
            s_b,
            t,
            t_a,
            t_b,
            t_u,
            t_u_a,
            t_u_b,
            s_ss,
            s_ss_a,
            s_ss_b,
        }
    }
}
