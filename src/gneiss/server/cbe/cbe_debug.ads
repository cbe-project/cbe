
with System;

package Cbe_Debug is

   procedure Print_C_String (S : System.Address;
                             L : Natural) with
      Export,
      Convention => C,
      External_Name => "print_cstring";

end Cbe_Debug;
