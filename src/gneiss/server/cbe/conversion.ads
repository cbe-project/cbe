
package Conversion with
   SPARK_Mode
is

   generic
      type T1 is private;
      type T2 is private;
   procedure Convert (V1 :     T1;
                      V2 : out T2);

   generic
      type T1 is private;
      type T2 is private;
      with procedure Collect (V2 : T2);
   procedure Pass_In (V1 : T1);

   generic
      type T1 is private;
      type T2 is private;
      with procedure Collect (V2 : out T2);
   procedure Pass_Out (V1 : out T1);

end Conversion;
