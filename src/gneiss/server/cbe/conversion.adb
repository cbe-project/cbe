
package body Conversion with
   SPARK_Mode
is

   procedure Convert (V1 :     T1;
                      V2 : out T2) is
      --  pragma Compile_Time_Error (T1'Size = T2'Size, "Cannot convert between differently sized types");
      V_Temp : T2 with
         Address => V1'Address;
   begin
      V2 := V_Temp;
   end Convert;

   procedure Pass_In (V1 : T1) is
      --  pragma Compile_Time_Error (T1'Size = T2'Size, "Cannot convert between differently sized types");
      V_Temp : T2 with
         Address => V1'Address;
   begin
      Collect (V_Temp);
   end Pass_In;

end Conversion;
