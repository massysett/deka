#include "decNumber.h"

int m_decNumberIsCanonical (const decNumber* dn) {
  return 1;
}

int m_decNumberIsFinite (const decNumber* dn) {
  return decNumberIsFinite(dn);
}

int m_decNumberIsInfinite (const decNumber* dn) {
  return decNumberIsInfinite(dn);
}

int m_decNumberIsNaN (const decNumber* dn) {
  return decNumberIsNaN(dn);
}

int m_decNumberIsNegative (const decNumber* dn) {
  return decNumberIsNegative(dn);
}

int m_decNumberIsQNaN (const decNumber* dn) {
  return decNumberIsQNaN(dn);
}

int m_decNumberIsSNaN (const decNumber* dn) {
  return decNumberIsSNaN(dn);
}

int m_decNumberIsSpecial (const decNumber* dn) {
  return decNumberIsSpecial(dn);
}

int m_decNumberIsZero (const decNumber* dn) {
  return decNumberIsZero(dn);
}

int m_decNumberRadix (const decNumber* dn) {
  return decNumberRadix(dn);
}
