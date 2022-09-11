with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA;


generic
    type T_Id is private;
	type T_Data is private;

package TREE is

    type T_Tree is  private;

    Empty_Tree : exception;

    package LCA_Integer_data is
            new LCA (T_Cle => Integer, T_Donnee => T_data);
    use LCA_Integer_data;
    package LCA_Integer_id is
            new LCA (T_Cle => Integer, T_Donnee => T_id);
    use LCA_Integer_id;


	-- Initialiser un Tree.
    procedure Initialize(Tree: out T_Tree) with
		Post => Is_empty (Tree);

	-- Tester si un arbre est vide
    function Is_Empty (Tree : T_Tree) return Boolean;

    -- Tester si un arbre est une feuille
    function Is_Leaf (Tree : T_Tree) return Boolean;

    -- Rentourner la clé de l'arbre
    function id( Tree :  T_Tree) return T_Id;

    -- Rentourner la donnée de l'arbre
    function data(Tree : T_Tree) return T_Data;

    -- Rentourner le fils droit de l'arbre
    function right (Tree : in T_Tree) return T_Tree;

    -- Rentourner le fils gauche de l'arbre
    function left (Tree : in T_Tree) return T_Tree;

   --Créer un noeud avec deux arbres en entré. Le noeud sera crée sur left et right sera supprimé pour libérer
   --la mémoire.
    procedure Create_Node (Tree : out T_tree; left :  in T_Tree; right : in T_Tree; id : in T_id; data: in T_data ) ;
    --Post => key(left)=key(left)'old+key(right)'old;

   -- Créer un arbre constitué d'une seule feuille
    procedure Create_Leaf (Tree : out T_Tree ; Id : in T_id; Data : in T_data);

    -- Trouver le parcours infixe d'un arbre
    procedure Parcours_Infixe( Tree : in T_Tree ; infixe : out Unbounded_String) ;

    -- Désallouer la mémoire
	procedure Free_Tree (Tree : in out T_Tree) ;

    -- Créer un arbre à partir d'un parcours infixe et une liste de données
    procedure huffman_tree ( tree: in out T_tree ; parcours: in LCA_Integer_data.T_lca ;  characters : in LCA_Integer_id.T_lca; c0, c1: in T_data ; ip ,ic : in out integer);

private

    type T_Node;

    type T_Tree is access T_Node;

    type T_Node is record
        Left : T_Tree;
        Right : T_Tree;
        Data : T_data;
        id : T_id;
    end record;

end TREE;
