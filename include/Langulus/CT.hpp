#pragma once
#include "CTTI.hpp"


#define LANGULUS_CTTI_CONCEPT(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = ((CTTI::NAME<T>::Value or T::CTTI_##NAME::Value) and ...); \
      template<class...T> \
      concept Not##NAME = ((not NAME<T>) and ...); \
   }

LANGULUS_CTTI_CONCEPT(Typelist);
LANGULUS_CTTI_CONCEPT(Void);
LANGULUS_CTTI_CONCEPT(Array);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Sparse);