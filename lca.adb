with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda:=NULL;
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return Sda=NULL;
	end;


    function Taille (Sda : in T_LCA) return Integer is
    begin
        if Est_Vide(Sda) then
            return 0;
        end if;
        return 1+ Taille(Sda.All.Suivant);
	end Taille;

    -- On parcourt récursivement la liste jusqu'à trouver l'élément dont la clé est égale à la clé recherchée
    -- Si on la trouve, on change sa donnée. Sinon on crée un nouveau élément.
	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Donnee : in T_Donnee) is
	begin
        if Est_Vide(Sda) then
            Sda := new T_Cellule'(Cle, Donnee, NULL);
        elsif Sda.All.Cle = Cle then
            Sda.All.Donnee :=Donnee;
        else
            Enregistrer (Sda.All.Suivant, Cle, Donnee);
        end if;
	end Enregistrer;

    -- On parcourt récursivement la liste jusqu'à trouver l'élément dont la clé est égale à la clé recherchée
    -- Si on la trouve, on retourne vrai. Sinon on retourne faux
    function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
    begin
        if Est_Vide(Sda) then
            return False;
        end if;
        return (sda.All.Cle = Cle or Cle_Presente(Sda.All.Suivant, Cle));
    end Cle_Presente;

    -- On parcourt récursivement la liste jusqu'à trouver l'élément dont la clé est égale à la clé recherchée
    -- Si on la trouve, on renvoie la donnée correspondante . Sinon on lève une exception
    function La_Donnee (Sda : in T_LCA ; Cle : in T_Cle) return T_Donnee is
    begin
        if Est_Vide(Sda) then
            raise Cle_Absente_Exception;
        elsif Sda.All.Cle=Cle then
            return Sda.All.Donnee;
        else
            return La_Donnee(Sda.All.Suivant, Cle);
        end if;
    end La_Donnee;

    -- On parcourt récursivement la liste jusqu'à trouver l'élément dont la clé est égale à la clé recherchée
    -- Si on la trouve, on libère sa case. Sinon on lève une exception
    procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
        Cellule: T_LCA;
    begin
        if  Est_Vide(Sda) then
            raise Cle_Absente_Exception;
        elsif Sda.All.Cle= Cle then
            Cellule := Sda;
            Sda := Cellule.All.Suivant;
            Free(Cellule);
        else
            Supprimer(Sda.All.Suivant, Cle);
        end if;
	end Supprimer;


    procedure Vider (Sda : in out T_LCA) is
    begin
        -- On parcourt la liste Sda jusqu'au dernier élément
        -- Ensuite on libère case par case commençant par la fin
        if not Est_Vide(Sda) then
            Vider(Sda.All.Suivant);
            Free(Sda);
        end if;

	end Vider;


	procedure Pour_Chaque (Sda : in T_LCA) is
    begin
        if not Est_Vide(Sda) then
            -- On appelle la procédure Traiter dans un nouveau bloc pour que si l'on trouve une exception on n'arrête pas le programme
            begin
                Traiter (Sda.All.Cle, Sda.All.Donnee);
            exception
                when others => Put_Line("Exception");
            end;
            Pour_Chaque(Sda.All.Suivant);
        end if;
   end Pour_Chaque;

    function La_Cle (Sda : in T_LCA) return T_Cle is
    begin
        return(Sda.all.cle);
    end La_Cle;

   function Suivant (Sda : in T_LCA) return T_Lca is
   begin
      return(Sda.all.suivant);
   end Suivant;

   procedure Ajouter_au_debut (Sda : in out T_LCA ; Cle : in T_Cle ; Donnee : in T_Donnee) is
   begin
      Sda := new T_Cellule'(Cle, Donnee, Sda);
   end Ajouter_au_debut;

   procedure Double (Sda : in out T_LCA) is
   begin
      if Sda.all.suivant = null then
         Sda.all.suivant := New T_Cellule'(Sda.all.Cle, Sda.all.Donnee, NULL);
      else
         double(Sda.all.suivant);
      end if;
   end double;


end LCA;
