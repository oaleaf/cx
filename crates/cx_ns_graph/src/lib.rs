use petgraph::graph::DiGraph;
use petgraph::stable_graph::NodeIndex;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CXNamespace(usize);

impl CXNamespace {
    fn to_node_index(self) -> NodeIndex<usize> {
        NodeIndex::new(self.0)
    }

    fn from_node_index(index: NodeIndex<usize>) -> CXNamespace {
        CXNamespace(index.index())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CXCyclicNamespace(CXNamespace);

impl CXCyclicNamespace {
    pub fn get(self) -> CXNamespace {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CXNamespaceGraphNode;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct CXNamespaceDependency;

pub struct CXNamespaceGraph {
    graph: DiGraph<CXNamespaceGraphNode, CXNamespaceDependency, usize>,
}

impl CXNamespaceGraph {
    pub fn new() -> Self {
        Self {
            graph: DiGraph::default(),
        }
    }

    pub fn add_namespace(&mut self) -> CXNamespace {
        let index = self.graph.add_node(CXNamespaceGraphNode);
        CXNamespace::from_node_index(index)
    }

    pub fn add_namespace_dependency(&mut self, namespace: CXNamespace, dependency: CXNamespace) {
        self.graph.add_edge(
            dependency.to_node_index(),
            namespace.to_node_index(),
            CXNamespaceDependency,
        );
    }

    pub fn tree_shake(&mut self, namespace: CXNamespace) {
        let result = petgraph::algo::floyd_warshall(&self.graph, |_| 1usize).unwrap();

        let mut to_remove = Vec::new();
        for node in self.graph.node_indices() {
            if result[&(node, namespace.to_node_index())] == usize::MAX {
                to_remove.push(node);
            }
        }

        for node in to_remove {
            self.graph.remove_node(node);
        }
    }

    pub fn transversal_route(&self) -> Result<Vec<CXNamespace>, CXCyclicNamespace> {
        let result = petgraph::algo::toposort(&self.graph, None)
            .map_err(|c| CXCyclicNamespace(CXNamespace::from_node_index(c.node_id())))?;

        Ok(result
            .into_iter()
            .map(|node| CXNamespace::from_node_index(node))
            .collect())
    }
}
