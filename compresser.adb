with ada.Command_Line ; Use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with LCA;
with TREE;

procedure Compresser is

    type T_Octet is mod 2 ** 8;	-- sur 8 bits
    for T_Octet'Size use 8;

    -------- Instanciation des modules ------------------
    package Tree_Octet_Integer is
            new TREE (T_Id => T_Octet, T_Data => Integer);
    use Tree_Octet_Integer;

    package LCA_Integer_Octet is
            new LCA (T_Cle => T_Octet, T_Donnee => Integer);
    use LCA_Integer_Octet;

    package LCA_Integer_Tree is
            new LCA (T_Cle => Integer, T_Donnee => T_Tree);
    use LCA_Integer_Tree;
    -----------------------------------------------------

    MISSING_ARGUMENT : exception;
    TOO_MANY_ARGUMENT : exception;
    UNKNOWN_ARGUMENT : exception;


    File_Name : Unbounded_String;
    File      : Ada.Streams.Stream_IO.File_Type;
    File_compressed : Ada.Streams.Stream_IO.File_Type;
    S : Stream_Access;
    S_compressed         : Stream_Access;
    Octet     : T_Octet;
    Verbose : Boolean := False;
    Code : Unbounded_String;
    IntVal : Integer;

    Liste_Octet : LCA_Integer_Octet.T_LCA;
    Liste_Tree : LCA_Integer_Tree.T_LCA;
    Position_octet : LCA_Integer_Octet.T_LCA;

    Tree : T_Tree;
    cpt : Integer := 0;
    -------------------------------------------------------------
    ----- Convertie une liste d'octet en liste de feuille -------
    -------------------------------------------------------------
    Procedure To_leaf(Liste_Tree : out LCA_Integer_Tree.T_Lca ; Liste_Octet : in LCA_Integer_Octet.T_Lca ; i : in Integer) is
        Leaf : T_Tree ;
    begin
        if not Est_Vide(Liste_Octet) then
            Create_Leaf(Leaf,La_Cle(Liste_Octet),La_Donnee(Liste_Octet,La_Cle(Liste_Octet)));
            Enregistrer(Liste_Tree,i,Leaf);
            To_Leaf(Liste_Tree,Suivant(Liste_Octet),i+1);
        end if;
        --Free_tree(Leaf);
    end To_leaf;
    -------------------------------------------------------------

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
            Put("'" & Character'val(id(tree)) & "'");
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

    -------------------------------------------------------------
    ------------------- Afficher la LCA_Octet -------------------
    -------------------------------------------------------------
    procedure Afficher_LCA_Octet (S : in T_Octet; N: in Integer) is
    begin
        Put (Character'Val(S));
        Put (" : ");
        Put (N);
        New_Line;
    end Afficher_LCA_Octet;

    procedure Afficher_LCA_Octet is
            new LCA_Integer_Octet.Pour_Chaque (Afficher_LCA_Octet);
    -------------------------------------------------------------

    -------------------------------------------------------------
    ------------------- Afficher la LCA_Tree --------------------
    -------------------------------------------------------------
    procedure Afficher_LCA_Tree (S : in Integer; N: in T_Tree) is
    begin
        Put (S);
        Put (" : ");
        Print_Tree(N,0);
        New_Line;
    end Afficher_LCA_Tree;

    procedure Afficher_LCA_Tree is
            new LCA_Integer_Tree.Pour_Chaque (Afficher_LCA_Tree);
    -------------------------------------------------------------

    -------------------------------------------------------------
    ----- Trouver la plus petite occurence et son indice --------
    -------------------------------------------------------------
    procedure min_Tree(Liste_Tree : in LCA_Integer_Tree.T_Lca ; Tree : in out T_Tree ; Cle : in out Integer) is
    begin
        if not Est_Vide(Liste_Tree) then
            if data(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))) < data(tree) then
            Cle := LCA_Integer_Tree.La_Cle(Liste_Tree);
                Create_Node(Tree,left(LCA_Integer_Tree.La_Donnee(Liste_Tree,Cle)),right(LCA_Integer_Tree.La_Donnee(Liste_Tree,Cle)),id(LCA_Integer_Tree.La_Donnee(Liste_Tree,Cle)),data(LCA_Integer_Tree.La_Donnee(Liste_Tree,Cle)));
            end if;
            min_Tree(LCA_Integer_Tree.Suivant(Liste_Tree),Tree , Cle);
        end if;

    end min_Tree;
    -------------------------------------------------------------

    -------------------------------------------------------------
    --------------- Construire l'arbre de Huffman ---------------
    -------------------------------------------------------------
    procedure Huffman_Tree (Liste_Tree : in out LCA_Integer_Tree.T_Lca ; Tree : out T_Tree) is
        Tree_g : T_Tree;
        Tree_d : T_Tree;
        Node : T_Tree;
        Cle : Integer;
        Taille : Integer := LCA_Integer_Tree.Taille(Liste_Tree);
    begin
        while LCA_Integer_Tree.Taille(Liste_Tree)>1 loop

            --on suppose que la 1er valeur est la plus petite
            Create_Node(Tree_g,left(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),right(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),id(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),data(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))));
            Cle := La_Cle(Liste_Tree);
            --Tree_g est le premier minimum
            min_Tree(Liste_Tree,Tree_g,Cle);
            Supprimer(Liste_Tree,Cle);

            --on suppose que la 1er valeur est la plus petite
            Create_Node(Tree_d,left(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),right(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),id(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))),data(LCA_Integer_Tree.La_Donnee(Liste_Tree,LCA_Integer_Tree.La_Cle(Liste_Tree))));
            Cle := La_Cle(Liste_Tree);
            --Tree_d est le second minimum
            min_Tree(Liste_Tree,Tree_d,Cle);
            Supprimer(Liste_Tree,Cle);

            Create_Node(Node,Tree_g,Tree_d,T_Octet(27),data(Tree_g)+data(Tree_d));
            Taille := Taille+1;

            Enregistrer(Liste_Tree,Taille,Node);

        end loop;
        Create_Node(Tree,left(Node),right(Node),T_Octet(27),data(Node));
    end Huffman_Tree;
    -------------------------------------------------------------

    -------------------------------------------------------------
    ------ position de chaques caracteres dans l'arbre ----------
    -------------------------------------------------------------
    procedure position(tree : in T_Tree; Liste : in out LCA_Integer_Octet.T_LCA ; cpt : in out integer) is
    begin
        if is_leaf(Tree) then
            LCA_Integer_Octet.Enregistrer(liste,id(Tree),cpt);
            cpt:=cpt+1;
        else
            position(left(Tree),Liste,cpt);
            position(right(Tree),Liste,cpt);
        end if;
    end position;
    -------------------------------------------------------------

    ------------------------ Huffman Code -----------------------
    -- Renvoie le codage Huffman d'une certaine clé "id" --
    -------------------------------------------------------------
    -- Utilisation d'un tableau dans la fonction
    type tab is array (1..100) of Unbounded_String;
    t : tab ;
    -------------------------------------------------------------
    procedure Huffman_Code(Tree : in T_Tree; id: in T_Octet; t: in out tab; index: in Integer; code : in out unbounded_string) is
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
            if Tree_Octet_Integer.id(tree) = id then
                for i in 1..(index-1) loop
                    code := code & t(i);
                end loop;
            end if;
      end if;
   end Huffman_code;
    -------------------------------------------------------------

    -------------------------------------------------------------
    --Ecrire les octets dans l'ordre dans le fichier de sortie --
    -------------------------------------------------------------
    procedure Ecrire_Lca_Octet (S : in T_Octet; N: in Integer) is
    begin
        T_Octet'write(S_compressed,S);
    end Ecrire_LCA_Octet;

    procedure Ecrire_Lca_Octet is
            new LCA_Integer_Octet.Pour_Chaque (Ecrire_Lca_Octet);
    -------------------------------------------------------------

    -------------------------------------------------------------
    ------------- Affichage de la table d'Huffman ---------------
    -------------------------------------------------------------
    procedure Ecrire_Table_Huffman (S : in T_Octet; N: in Integer) is
    begin
        Put (Character'Val(S));
        Put (" : ");
        Code := To_Unbounded_String("");
        Huffman_Code(Tree, S, t, 1, Code);
        Put(To_String(Code));
        New_Line;
    end Ecrire_Table_Huffman;

    procedure Ecrire_Table_Huffman is
            new LCA_Integer_Octet.Pour_Chaque (Ecrire_Table_Huffman);
    -------------------------------------------------------------

    -------------------------------------------------------------
    -------------- Compléter le dernier octet -------------------
    -------------------------------------------------------------
    procedure complete0 (Code : in out Unbounded_String) is
    begin
        if Length(Code) mod 8 /= 0 then
            Code := Code & To_Unbounded_String("0");
            complete0(Code);
        end if;
    end complete0;
    -------------------------------------------------------------

   -----------
   -- DEBUT --
   -----------

begin

    if Argument_Count = 0 then
        raise MISSING_ARGUMENT;
    elsif Argument_Count = 1 then
        File_Name := To_Unbounded_String(Argument(1));
    elsif Argument_Count = 2 then
        if Argument(1) = "-b" or Argument(1) = "--bavard" then
            verbose := True;
            New_Line;
            Put("==> VERBOSE MODE");
            File_Name := To_Unbounded_String(Argument(2));
        else
            raise UNKNOWN_ARGUMENT;
        end if;
    else
        raise TOO_MANY_ARGUMENT;
    end if;


    ---------- Lecture du contenu du fichier -----------
    if verbose then
        New_Line;
        New_Line;
        Put("==> Lecture du fichier source");
    end if;

    -----------------------------------------------------
    --------------- Ouverture du fichier ----------------
    -----------------------------------------------------
    Open(File, In_File, To_String(File_Name));
    -----------------------------------------------------

    -----------------------------------------------------
    ------- Liste_Octet : Octets et leurs occurences ----
    -----------------------------------------------------
    Initialiser(Liste_Octet);
    S := Stream(File);
    while not End_Of_File(File) loop
        Octet := T_Octet'Input(S);
        if Cle_Presente(Liste_Octet,Octet) then
            Enregistrer(Liste_Octet,Octet,La_Donnee(Liste_Octet,Octet)+1);
        else
            Enregistrer(Liste_Octet,Octet,1);
        end if;
    end loop;
    ----- Ajout du caractere de fin de texte : EOF -------
    Enregistrer(Liste_Octet,T_Octet(3),0);
    -----------------------------------------------------

    -----------------------------------------------------
    --------------- Fermeture du fichier ----------------
    -----------------------------------------------------
    Close (File);
    -----------------------------------------------------

    -----------------------------------------------------
    ------------- Creation de l'arbre -------------------
    -----------------------------------------------------
    if verbose then
        New_Line;
        New_Line;
        Put("==> Creation de l'abre");
    end if;

    -----------------------------------------------------
    -------- Mise en place de toutes les feuilles -------
    --------- Liste_Tree contenant des feuilles ---------
    --------- de cle octet et de donnees entieres -------
    To_leaf(Liste_Tree,Liste_Octet,1);
    -----------------------------------------------------

    --------------- Creation de l'abre ------------------
    Huffman_Tree(Liste_Tree, Tree);
    -----------------------------------------------------

    -----------------------------------------------------
    ---------- Affichage de l'arbre de Huffman ----------
    -----------------------------------------------------

   if verbose then
        New_Line;
        New_Line;
        Put("==> Arbre :");
        New_Line;
        Print_tree(Tree,0);
    end if;

    -- LCA avec les caracteres dans l'odre d'apparition dans
    -- l'arbre de Huffman
    cpt := 1;
    position(Tree,Position_octet,cpt);

    -----------------------------------------------------
    ---------- Affichage de la table de Huffman ---------
    -----------------------------------------------------

    if verbose then
        New_Line;
        New_Line;
        Put("==> Table de Huffman :");
        New_Line;
        Ecrire_Table_Huffman(Position_octet);
    end if;

    -----------------------------------------------------
    ----- Suppression du caractere de fin de texte ------
    --------- et ajout de sa position au début ----------
    -----------------------------------------------------
    LCA_Integer_Octet.Ajouter_au_debut(Position_octet, T_Octet(LCA_Integer_Octet.La_Donnee(Position_octet,T_Octet(3))), 0);
    LCA_Integer_Octet.Supprimer(Position_octet,T_Octet(3));
    -----------------------------------------------------

    -----------------------------------------------------
    --------- Dedoublement du dernier caractere ---------
    -----------------------------------------------------
    LCA_Integer_Octet.Double(Position_octet);
    -----------------------------------------------------

    ---- Ecriture des octets dans le nouveau fichier ----

    if verbose then
        New_Line;
        New_Line;
        Put("==> Creation du fichier compresse");
    end if;

    -----------------------------------------------------
    --------------- Ouverture du fichier ----------------
    -----------------------------------------------------
    Create (File_compressed, Out_File, To_String(File_Name)&".hff");
    S_compressed := Stream (File_compressed);
    -----------------------------------------------------

    -----------------------------------------------------
    ---------------- Ecriture des octets ----------------
    -----------------------------------------------------

    ----- Ecriture des caracteres dans le fichier de ----
    ------ dans l'ordre d'appartion dans l'arbre --------
    Ecrire_Lca_Octet(Position_octet);

    ----------- Ecriture du parcourt infixe -------------
    Code := To_Unbounded_String("");
    Parcours_Infixe(Tree, Code);
    Code := Code & To_Unbounded_String("1");

    ---- Encodage du texte suivant les code Huffman ------

    -----------------------------------------------------
    ---------- Ouverture du fichier a compresser --------
    -----------------------------------------------------
    Open(File, In_File, To_String(File_Name));
    -----------------------------------------------------
    -- Ecrire le texte encode suivant Huffman dans une
    -- chaine de caracteres "Code"
    S := Stream(File);
    while not End_Of_File(File) loop
        Octet := T_Octet'Input(S);
        Huffman_Code(Tree, Octet, t, 1, Code);
    end loop;

    -----------------------------------------------------
    ---------- Ouverture du fichier a compresser --------
    -----------------------------------------------------
    Close (File);
    -----------------------------------------------------

    -------------- Completer le dernier octet ------------
    complete0(Code);
    ------------------------------------------------------

    --------- Ecriture du encode dans le fichier ---------
    for i in 1..(length(Code)/8) loop
        IntVal := Integer'Value("2#"&To_String(Code)(i*8-7..8*i)&"#");
        T_Octet'write(S_compressed,T_Octet(IntVal));
    end loop;
    -----------------------------------------------------

    -----------------------------------------------------
    --------------- Fermeture du fichier ----------------
    -----------------------------------------------------
    Close (File_compressed);
    -----------------------------------------------------

    ---------
    -- FIN --
    ---------
    if verbose then
        New_Line;
        Put("==> Fin");
    end if;

    -------------- Desalloaction de la mémoire ----------
    LCA_Integer_Octet.Vider(Liste_Octet);
    LCA_Integer_Tree.Vider(Liste_Tree);
    LCA_Integer_Octet.Vider(Position_octet);
    Free_Tree(Tree);

exception
   when ADA.IO_EXCEPTIONS.NAME_ERROR =>
      New_Line;
      Put_line("!!! Erreur fichier inexistant !!!") ;
      New_Line;

   when COMPRESSER.MISSING_ARGUMENT =>
      New_Line;
      Put_line("!!! Erreur absence d'argument. Exemple syntaxe : ./compresser -b fichier.txt !!!") ;
      New_Line;

   when COMPRESSER.TOO_MANY_ARGUMENT =>
      New_Line;
      Put_line("!!! Erreur trop d'arguments en entree !!!") ;
      New_Line;

   when COMPRESSER.UNKNOWN_ARGUMENT =>
      New_Line;
      Put_line("!!! Erreur argument inconnu !!!") ;
      New_Line;

end Compresser;
