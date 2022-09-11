with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LCA;
with TREE;

procedure TEST_TREE is

    ---------------------------------------------------------------
    package Tree_String_Integer is
            new TREE (T_Id => Unbounded_String, T_Data => Integer);
    use Tree_String_Integer;
    ---------------------------------------------------------------

    ---------------------------------------------------------------
    ---------------------------------------------------------------
    ---------------------------------------------------------------
    procedure Afficher1 (N: in Integer; S : in integer) is
	begin
        Put (N, 1);
        Put (" : ");
        Put (S,1);
        New_Line;
    end Afficher1;

	procedure Afficher is
		new LCA_Integer_data.Pour_Chaque (Afficher1);
    ---------------------------------------------------------------

    ---------------------------------------------------------------
    ---------------------------------------------------------------
    ---------------------------------------------------------------
    procedure Afficher2 (N: in Integer; S : in Unbounded_String) is
	begin
        Put (N, 1);
        Put (" : ");
        Put (To_String (S));
        New_Line;
    end Afficher2;

	procedure Afficher is
		new LCA_Integer_id.Pour_Chaque (Afficher2);
    ---------------------------------------------------------------


    type tab is array (1..100) of Unbounded_String;
    t : tab ;

    ---------------------------------------------------------------
    ------------- Fournir le codage Huffman d'une clé --------------
    ---------------------------------------------------------------
    -- in Tree : Arbre de Huffman
    -- in Id : La clé de la feuille dont on veut le codage Huffman
    -- in out T : Tableau permettant le stockage du codage Huffman
    -- in out Index : Entier indiquant l'intervalle d'indices du tableau T
    -- contenant le codage Huffman de la feuille courante
    -- in out Code : Unbounded_String contenant le code Huffman de Id
    procedure Huffman_Code(Tree : in T_Tree; id: in unbounded_string; t: in out tab; index: in Integer; code : in out unbounded_string) is
    begin
        if not is_empty(left (tree)) then
            t(index) := To_Unbounded_String("0");
            Huffman_Code(left(tree),id,t, index+1, code);
        end if;
        if not is_empty(right (tree)) then
            t(index) := To_Unbounded_String("1");
            Huffman_Code(right(tree), id, t, index+1, code);
        end if;
        if is_leaf(tree)  then
            if Tree_String_Integer.id(tree) = id then
                for i in 1..(index-1) loop
                    code := code & t(i);
                end loop;

            end if;
        end if;
    end Huffman_Code;

    -------------------------------------------------------------
    ------------------------ Print Tree -------------------------
    -------------------------------------------------------------
    procedure print_tree(Tree: in T_tree; espace : in integer) is
        espace_gauche: integer;
        espace_droit: integer;
    begin
        Put("(");
        Put(data(tree),1);
        Put(")");
        if is_leaf(tree)  then
            Put("'" & To_String(id(tree)) & "'");
        else
            New_line;
            for i in 1..espace loop
                put(" |     ");
            end loop;
            Put(" \--0--");
            espace_gauche := espace +1;
            print_tree(left(tree), espace_gauche);
            New_line;
            for i in 1..espace loop
                put(" |     ");
            end loop;
            Put(" \--1--");
            espace_droit := espace +1;
            print_tree(right(tree), espace_droit);
        end if;
    end print_tree;
    -------------------------------------------------------------


    tree_1, tree_2, tree_3: T_Tree;
    tree_4, tree_5: T_Tree;
    c : unbounded_string :=  To_Unbounded_String("");
    code : unbounded_string :=  To_Unbounded_String("");

    L1 :LCA_Integer_data.T_LCA;
    L2 :LCA_Integer_id.T_LCA;
    Created_Tree : T_tree;
    ip: Integer := 0;
    ic : Integer := 0;
begin
    Put_line ("--- test initialize ---");
    Initialize(tree_1);
    Initialize(tree_2);

    pragma Assert (Is_Empty (tree_1));
    pragma Assert (Is_Empty (tree_2));

    Put_line ("--- test create leaf & node ---");
    Create_Leaf(tree_1, To_Unbounded_String("a"),1);
    Create_Leaf(tree_2, To_Unbounded_String("b"),2);

    Create_Node(tree_3, tree_1, tree_2, id(tree_1) & id(tree_2), data(tree_1)+data(tree_2)); --supprime tree_2 et ajoute la feuille b dans tree_1
    pragma Assert (data (tree_3) = 3);
    pragma Assert (id (tree_3) = "ab");

    Create_Leaf(tree_4, To_Unbounded_String("d"),4);

    Create_Node(tree_5, tree_3, tree_4, id(tree_3) & id(tree_4), data(tree_3)+data(tree_4)); --supprime tree_2 et ajoute la feuille b dans tree_1
    pragma Assert (data (tree_5) = 7);
    pragma Assert (id (tree_5) = "abd");
    print_tree(tree_5, 0);
    New_line;

    Put_line ("--- test parcours_infixe ---");
    Parcours_Infixe(tree_5, code);
    Put(To_String(code));
    code := To_unbounded_string("");
    New_line;
    Put_line("Doit être égale à 0011");

    -- Codage Huffman
    Put_line ("--- test codage Huffman ---");
    Huffman_Code(tree_5, To_Unbounded_String("a"), t,1 , code);
    put(To_String(code));
    new_line;
    code := To_unbounded_string("");
    Huffman_Code(tree_5, To_Unbounded_String("b"), t,1, code);
    put(To_String(code));
    new_line;
    code := To_unbounded_string("");
    Huffman_Code(tree_5, To_Unbounded_String("d"), t,1, code);
    put(To_string(code));
    new_line;


    -- Huffman Tree
    -- Initialisation de la liste
    LCA_Integer_data.Initialiser(L1);
    LCA_Integer_id.Initialiser(L2);

    -- L1 : 0011 parcours
    LCA_Integer_data.Enregistrer(L1, 0, 0);
    LCA_Integer_data.Enregistrer(L1, 1, 0);
    LCA_Integer_data.Enregistrer(L1, 2, 1);
    LCA_Integer_data.Enregistrer(L1, 3, 1);

    -- L2 : les caractères
    LCA_Integer_id.Enregistrer(L2, 0, To_Unbounded_String("a"));
    LCA_Integer_id.Enregistrer(L2, 1, To_Unbounded_String("b"));
    LCA_Integer_id.Enregistrer(L2, 2, To_Unbounded_String("d"));

    Afficher(L1);
    Afficher(L2);

    Initialize(Created_Tree);
    Create_Leaf(Created_Tree,To_unbounded_string(" "), 0);
    huffman_tree ( Created_Tree,L1, L2, 0, 1, ip ,ic);
    print_tree(Created_Tree,0);
    New_line;

    -- Désallocation de la mémoire
    LCA_Integer_data.Vider(L1);
    LCA_Integer_id.Vider(L2);

    --Free_Tree(tree_1);
    --Free_Tree(tree_2);
    --Free_Tree(tree_4);
    --Free_Tree(tree_3);
    free_tree(tree_5);
    Free_Tree(Created_Tree);

    New_line;
    Put_Line(" All tests passed successfuly !");


end Test_TREE;
