#include <stdio.h>

int main(int argc, char **argv) {
   #ifdef SOMETHING1
   printf("Compile with SOMETHING1\n");

   #ifdef SOMETHING2
   printf("Also compile with SOMETHING2\n");
   #endif

   #else
   printf("Compile without SOMETHING1\n");
   #endif

   printf("General behavior.");

   #ifdef SOMETHING3
   printf("Compile with SOMETHING3");
   #endif

   return 0;
}
