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
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);
LANGULUS_CTTI_CONCEPT(Volatile);

namespace Langulus::CT
{

   /// Check if all T are sparse                                              
   template<class...T>
   concept Sparse = ((CTTI::Sparse<T>::Value or T::CTTI_Sparse::Value) and ...);

   /// Check if all T are dense                                               
   template<class...T>
   concept Dense = ((not Sparse<T>) and ...);

   /// Check if all T are constant-qualified                                  
   template<class...T>
   concept Constant = ((CTTI::Constant<T>::Value or T::CTTI_Constant::Value) and ...);

   /// Check if all T are not constant-qualified                              
   template<class...T>
   concept Mutable = ((not Constant<T>) and ...);

   /// Check if all T are either const- and/or volatile-qualified             
   template<class...T>
   concept Convoluted = ((Constant<T> or Volatile<T>) and ...);

   /// Check if none of T are const- and/or volatile-qualified                
   template<class...T>
   concept NotConvoluted = ((not Convoluted<T>) and ...);

} // namespace Langulus::CT
