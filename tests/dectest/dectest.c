#define DECNUMDIGITS 100

#include <decNumber.h>
#include <decContext.h>
#include <stdio.h>

int main (int argc, char** argv) {
  decContext ctxt;
  decNumber dn1;
  decNumber dn2;
  char str[100];

  decContextDefault(&ctxt, DEC_INIT_DECIMAL128);
  ctxt.digits = 4;
  decNumberFromString(&dn1, argv[1], &ctxt);
  printf("%s\n", decNumberToString(&dn1, str));
  printf("%u\n", ctxt.status);
  return 0;
}
