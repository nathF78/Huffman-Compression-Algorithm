with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body TREE is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => T_Node, Name => T_Tree);


   procedure Initialize(Tree: out T_Tree) is
   begin
      Tree:=NULL;
   end Initialize;

   function Is_Empty (Tree : T_Tree) return Boolean is
   begin
      return Tree=NULL;
   end Is_Empty;

    function Is_Leaf (Tree : T_Tree) return Boolean is
    begin
        if Is_Empty(tree) then
            return false;
        end if;
        return tree.all.Left = null and tree.all.right = null;
    end Is_Leaf;

    -- precondition : not is_empty(tree) ?
    function id( Tree :  T_Tree) return T_Id is
    begin
        if tree = null then
            raise Empty_Tree;
        end if;
        return tree.all.id;
    end id;

    function data(Tree : T_Tree) return T_Data is
    begin
        if tree = null then
            raise Empty_Tree;
        end if;
        return tree.all.data;
    end data;

    function right (Tree : in T_Tree) return T_Tree is
    begin
        if tree = null then
            raise Empty_Tree;
        end if;
        return tree.all.right;
    end right;


    function left (Tree : in T_Tree) return T_Tree is
    begin
        if tree = null then
            raise Empty_Tree;
        end if;
        return tree.all.left;
    end left;

    procedure Create_Node (Tree : out T_tree; left :  in T_Tree; right : in T_Tree; id : in T_id; data: in T_data ) is
    begin
      Tree := new T_Node'(left,right,data, id);
    end Create_Node;

    procedure Create_Leaf (Tree : out T_Tree ; Id : in T_id; Data :in T_data) is
    begin
        Tree := new T_Node'(null,null,Data,Id);
    end Create_Leaf;

    procedure Parcours_Infixe( Tree : in T_Tree ; infixe : out Unbounded_String) is
    begin
      if not is_empty(left (tree)) then
         infixe := infixe & To_Unbounded_String("0") ;
           Parcours_Infixe(tree.all.left, Infixe);
       end if;
        if not is_empty(right (tree)) then
           infixe :=  infixe & To_Unbounded_String("1");
           Parcours_Infixe(tree.all.right, Infixe);
        end if;
   end Parcours_Infixe;


    -- not is_empty(Tree)
    procedure Free_Tree (Tree : in out T_Tree) is
    begin
        if is_leaf(tree) then
            Free(Tree);
            Tree := null;
        else
            Free_tree(tree.all.left);
            Free_tree(tree.all.right);
        end if;
   end Free_Tree;

    procedure huffman_tree ( tree: in out T_tree ; parcours: in LCA_Integer_data.T_lca ;  characters : in LCA_Integer_id.T_lca; c0, c1: in T_data ; ip ,ic : in out integer) is
        data : T_id;
    begin
        data := LCA_Integer_id.La_Donnee(characters, ic);
        if LCA_Integer_data.Cle_Presente(parcours,ip) then
            if  LCA_Integer_data.La_Donnee(parcours, ip) = c1 then
                Free_Tree(tree);
                Create_Leaf(tree, data, c1);
            else
                create_node( tree.all.left, null, null, data, c0);
                ip := ip + 1;
                huffman_tree(tree.all.left, parcours, characters, c0,c1, ip, ic);
                ic := ic + 1;
                data := LCA_Integer_id.La_Donnee(characters, ic);
                create_node( tree.all.right, null, null, data, c1);
                ip := ip + 1;
                huffman_tree(tree.all.right, parcours, characters, c0,c1, ip, ic);
            end if;
        end if;
    end huffman_tree;

   end TREE;
