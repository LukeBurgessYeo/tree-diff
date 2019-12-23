# F# Tree Diff Algorithm

A simple algorithm to find the difference between two trees.

This algorithm works with labeled, unordered trees. Each node has a unique Id and contains a Value. The algorithm will detect if the Value of two nodes with the same Id has been updated as well as detecting inserted or deleted nodes.

__Input__: Two trees.

__Output__: A tree containing nodes from both input trees where each node is labeled to indicate whether the node has been inserted, deleted, updated or left unchanged.

Trees in this algorithm have the following type definition:

```
type Change<'T> =
    | Unchanged
    | Inserted
    | Deleted
    | Updated of 'T

type Node<'T> = {
    Id : int
    Value : 'T
    Modified : Change<'T>
    Children : Node<'T> list
}
```