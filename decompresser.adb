with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with ada.Command_Line ;     Use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with LCA;
with TREE;


procedure decompresser is

    ------- Usage: shows how the file should be used -----
    procedure Usage is
	begin
		New_Line;
		Put_Line ("Usage : " & Command_Name & "(optional :-b ou --bavard)" & " file_name");
		New_Line;
		Put_Line ("   -b ou --bavard  : prints more details");
		Put_Line ("   file_name : file with extension .hff that you want to decode");
		New_Line;
    end Usage;
    -----------------------------------------------------

    type T_Octet is mod 2 ** 8;
    for T_Octet'Size use 8;

    -------- Instanciation des modules ------------------
    package Tree_String_Integer is
            new TREE (T_Id => T_Octet, T_Data => Integer);
    use Tree_String_Integer;
    tree : T_tree;

    package LCA_code_char is
            new LCA (T_Cle => Unbounded_String, T_Donnee => T_octet);
    use LCA_code_char;
    -----------------------------------------------------

    --------------------- Huffman Code --------------------
    -- Renvoie le codage Huffman d'une certaine clé "id" --
    -------------------------------------------------------
    -- Utilisation d'un tableau dans la fonction
    type tab is array (1..100) of Unbounded_String;
    t : tab ;
    -----------------------------------------------------
    procedure Huffman_Code(Tree : in T_Tree; id: in T_octet; t: in out tab; index: in Integer; code : in out unbounded_string) is
    begin
        if not is_empty(left (tree)) then
            t(index) := To_Unbounded_String("0");
            Huffman_Code(left(tree),id,t, index+1, code);
        end if;
        if not is_empty(right (tree)) then
            t(index) := To_Unbounded_String("1");
            Huffman_Code(right(tree), id, t, index+1, code);
        end if;
        if is_leaf(tree) and Tree_String_Integer.id(tree) = id then
            for i in 1..(index-1) loop
                code := code & t(i);
            end loop;
        end if;
    end Huffman_Code;
    -----------------------------------------------------

    --------------------- Huffman Code --------------------
    -------- Affichage de la table d'Huffman --------------
    -------------------------------------------------------
	procedure Afficher (Code : in Unbounded_String; Char: in T_octet) is
	begin
		Put (''' & Character'Val(Char) & ''');
		Put (" --> ");
		Put (To_string(Code));
		New_Line;
	end Afficher;

	procedure Afficher is
		new LCA_code_char.Pour_chaque(Afficher);
    -------------------------------------------------------

    bavard : Boolean;
    File_Name : Unbounded_String;

    ---------------- Lecture du fichier -----------------
    File      : Ada.Streams.Stream_IO.File_Type;
    S         : Stream_Access;
    -----------------------------------------------------

    ------------- Ecriture dans le fichier --------------
    File_Decompressed      : Ada.Streams.Stream_IO.File_Type;
    S_Decompressed         : Stream_Access;
    -----------------------------------------------------

    Octet_Courant, bit     : T_Octet;
    Pos_Caractere_fin: T_Octet ;
    Octet_Precedent    : T_Octet := 0;

    lca_char : LCA_Integer_id.T_lca;
    lca_id : LCA_Integer_data.T_lca;
    l : LCA_code_char.T_LCA;

    continue : boolean;
    suite_zero_un : unbounded_string;

    nb_char : integer:=0;
    cpt : integer:=0;
    index : integer;

    ------------- Reconstruction de l'arbre -------------
    ic,ip : integer;
    -----------------------------------------------------

    ------------------ Codage Huffman -------------------
    code : Unbounded_String;
    -----------------------------------------------------

    ---------------- Traduction du texte ----------------
    indice_lecture:integer;
    -----------------------------------------------------
begin
    if Argument_Count <1 then
        Usage;
    else
        if Argument_Count =1 then
            File_name := To_Unbounded_String(Argument (1));
        else
            File_name := To_Unbounded_String(Argument (2));
        end if;
        bavard := (Argument_Count =2);

        -- Tester si le fichier est bien d'extension .hff (later)

        ---------- Lecture du contenu du fichier -----------

        -----------------------------------------------------
        --------------- Ouverture du fichier ----------------
        -----------------------------------------------------
        Open(File, In_File, To_String(File_Name));
        -----------------------------------------------------

        S := Stream(File);

        -----------------------------------------------------
        --------- Liste contenant les charactères -----------
        -----------------------------------------------------
        continue := true;
        LCA_Integer_id.initialiser(lca_char);

        -----------------------------------------------------
        -- Récupération du la position du caractère de fin --
        if not End_Of_File(file) then
            Pos_Caractere_fin := T_Octet'Input(S);
        end if;
        -----------------------------------------------------
        Put("=> Liste des octets représentant les caractères : ");
        while not End_Of_File(File) and continue loop
            Octet_Courant := T_Octet'Input(S);
            if Octet_Precedent /= Octet_Courant then
                nb_char:=nb_char+1;
                if nb_char = Integer(Pos_Caractere_fin) then
                    LCA_Integer_id.enregistrer(lca_char, nb_char, 3);
                    nb_char:=nb_char+1;
                end if;
                LCA_Integer_id.enregistrer(lca_char, nb_char, Octet_Courant);
                Octet_Precedent:= Octet_Courant;
            else
                continue := false;
            end if;
        end loop;
        Put("Success");
        New_line;
        -----------------------------------------------------
        --------- Chaîne de caractères contenant ------------
        ---------- Parcours infixe + Texte codé -------------
        -----------------------------------------------------
        suite_zero_un:= To_Unbounded_String("");
        while not End_Of_File(File) loop
            Octet_courant := T_Octet'Input(S);
            for i in 1..8 loop
                Bit := Octet_courant / 128;
                Octet_courant := (Octet_courant * 2);
                if bit = 1 then
                    suite_zero_un := suite_zero_un & To_Unbounded_String("1");
                else
                    suite_zero_un := suite_zero_un & To_Unbounded_String("0");
                end if;
            end loop;
        end loop;
        -----------------------------------------------------

        -----------------------------------------------------
        --------------- Fermeture du fichier ----------------
        -----------------------------------------------------
        Close (File);
        -----------------------------------------------------


    ----------------------------------------------------------------------------


        -----------------------------------------------------
        -------- Liste contenant le parcours infixe ---------
        -----------------------------------------------------
        Put("=> Liste Parcours Infixe : ");
        index := 1 ;
        LCA_Integer_data.initialiser(lca_id);
        while cpt <= nb_char loop
            if To_string(suite_zero_un)(index) ='1' then
                LCA_Integer_data.enregistrer(lca_id,index, 1);
                cpt := cpt + 1;
            else
                LCA_Integer_data.enregistrer(lca_id,index,0);
            end if;
            index := index + 1;
        end loop;
        suite_zero_un:= To_unbounded_string(To_string(suite_zero_un)((index+1)..length(suite_zero_un)));
        Put("Success");
        New_line;
        ----------------------------------------------------

        -----------------------------------------------------
        ------------- Reconstruction de l'arbre -------------
        -----------------------------------------------------
        Put("=> Reconstruction de l'arbre : ");
        ip := 1;
        ic := 1;
        Octet_Courant := 0;
        Create_Leaf(Tree,Octet_Courant, 0);
        huffman_tree ( tree, lca_id, lca_char, 0, 1, ip ,ic);
        Put("Success");
        New_line;
        -----------------------------------------------------

        -----------------------------------------------------
        --------- Liste Code Huffman <=> Character ----------
        -----------------------------------------------------
        Put("=> Liste Correspondance Code Huffman et Character : ");
        LCA_code_char.initialiser(l);
        for i in 1..LCA_Integer_id.Taille(lca_char) loop
            code := To_Unbounded_String("");
            Huffman_Code(Tree , LCA_Integer_id.La_Donnee(lca_char,i), t, 1, code );
            LCA_code_char.enregistrer(l,code,LCA_Integer_id.La_donnee(lca_char, i));
        end loop;
        Put("Success");
        New_line;
        -----------------------------------------------------


    ----------------------------------------------------------------------------


        ---- Ecriture du texte dans le nouveau fichier ------

        -----------------------------------------------------
        --------------- Ouverture du fichier ----------------
        -----------------------------------------------------
        File_name:=To_unbounded_string(to_string(File_name)(1..(Length(File_name)-4)));
        --File_name := To_Unbounded_String("new.txt");
        Create (File_Decompressed, Out_File, To_string(File_Name));
        -----------------------------------------------------

        -----------------------------------------------------
        ----------------- Ecriture du texte -----------------
        -----------------------------------------------------
        Put("=> Ecriture dans le fichier décompressé : ");
        S_Decompressed := Stream (File_Decompressed);
        indice_lecture:=1;
        for i in 1..Length(suite_zero_un) loop
            code := To_unbounded_string(To_string(suite_zero_un)(indice_lecture..i));
            if LCA_code_char.Cle_Presente(l,code) then
                T_Octet'Write(S_Decompressed, LCA_code_char.La_donnee(l, code));
                indice_lecture:=indice_lecture+length(code);
                code:=To_Unbounded_String("");
            end if;
        end loop;
        Put("Success");
        New_line;
        -----------------------------------------------------


        -----------------------------------------------------
        --------------- Fermeture du fichier ----------------
        -----------------------------------------------------
        Close (File_Decompressed);
        -----------------------------------------------------

    ----------------------------------------------------------------------------
        --------- Affichage de la table d'Huffman -----------
        Put_line(" File has been successfuly decoded !");
        Put_line(" You may consult it in " & To_string(File_name));

        if bavard then
            Put_line("Huffman Table :");
            Afficher(l);
            null;
        end if;

        -------------- Desalloaction de la mémoire ----------
        LCA_code_char.Vider(l);
        LCA_Integer_id.Vider(lca_char);
        LCA_Integer_data.Vider(lca_id);
        Free_Tree(Tree);
    end if;
end decompresser;
