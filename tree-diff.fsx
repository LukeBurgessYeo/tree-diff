// Tree-Diff implementation

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

/// Nodes are equal in all but Value
let (==) node1 node2 =
    node1.Id = node2.Id && node1.Children = node2.Children && node1.Value <> node2.Value

/// Combines the children of two nodes into a single list
let combine node1 node2 =
    let first = node1.Children |> List.map (fun x -> (Some x, node2.Children |> List.tryFind (fun y -> y.Id = x.Id)))
    let second = node2.Children |> List.map (fun x -> (node1.Children |> List.tryFind (fun y -> y.Id = x.Id)), Some x)
    first @ second |> List.distinct

let insert node = 
    { node with 
        Modified = Inserted
        Children =
            node.Children |> List.map (fun child -> { child with Modified = Inserted }) }

let delete node =
    { node with 
        Modified = Deleted
        Children =
            node.Children |> List.map (fun child -> { child with Modified = Deleted }) }

let update node value =
    { node with 
        Modified = Updated value }

let rec diff (oldNodeOpt, newNodeOpt) =
    match oldNodeOpt, newNodeOpt with
    | Some oldNode, None -> delete oldNode
    | None, Some newNode -> insert newNode
    | Some oldNode, Some newNode when oldNode = newNode -> oldNode
    | Some oldNode, Some newNode when oldNode == newNode -> update oldNode newNode.Value
    | Some oldNode, Some newNode ->
        { oldNode with
            Modified = if oldNode.Value = newNode.Value then Unchanged else Updated newNode.Value
            Children =
                combine oldNode newNode
                |> List.map diff }
    | None, None -> failwith "Must have at least one node to compare."

/// Compares two trees and returns a tree with the differences labeled
let compare oldTree newTree = diff (Some oldTree, Some newTree)


// -- Tests -- //

let printTree tree =
    let rec traverse node level =
        let spacing = List.init (level * 4) (fun _ -> " ") |> String.concat ""
        let nodeString = sprintf "(%i) %s - %A" node.Id node.Value node.Modified
        printfn "%s%s" spacing nodeString
        node.Children
        |> List.iter (fun child -> traverse child (level + 1))
    traverse tree 0

let tree0 = {
    Id = 0
    Value = "Root"
    Modified = Unchanged
    Children = [
        { Id = 1; Value = "A"; Modified = Unchanged; Children = [
            { Id = 3; Value = "D"; Modified = Unchanged; Children = [] }
        ] }
        { Id = 2; Value = "B"; Modified = Unchanged; Children = [] }
    ]
}

let tree1 = {
    Id = 0
    Value = "Root"
    Modified = Unchanged
    Children = [
        { Id = 2; Value = "C"; Modified = Unchanged; Children = [
            { Id = 3; Value = "D"; Modified = Unchanged; Children = [] }
        ] }
    ]
}

compare tree0 tree1
|> printTree