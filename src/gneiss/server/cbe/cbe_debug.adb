
with Componolit.Runtime.Debug;
with Componolit.Runtime.Strings;

package body Cbe_Debug is

   procedure Print_C_String (S : System.Address;
                             L : Natural)
   is
      Msg : constant String := Componolit.Runtime.Strings.Convert_To_Ada
                        (S, "Invalid String", L);
   begin
      Componolit.Runtime.Debug.Log_Debug (Msg);
   end Print_C_String;

end Cbe_Debug;
