///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::RTTI
{
   namespace Inner
   {
      #if LANGULUS_FEATURE(MANAGED_REFLECTION)
         template<unsigned ID_SIZE, unsigned PT_SIZE>
         struct MetaDataStructured_XY;
      #endif

      struct MetaDataNaked;
   }

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaData = Inner::MetaDataStructured_XY<2, 2>;
   #else
      using MetaData = Inner::MetaDataNaked;
   #endif
}

namespace Langulus::Anyness
{
   struct Any;
}

namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Resolvable<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Resolvable = Yes/No;` in T                 
   /// Either way, the type still has to have the required interface for this 
   /// to work                                                                
   template<class T>
   struct Resolvable {
      static constexpr bool Enabled = requires (T& a) {
         { a.GetType()     } -> ::std::same_as<RTTI::MetaData>;
         { a.GetResolved() } -> ::std::same_as<Anyness::Any>;
      };
   };
}

LANGULUS_CTTI_CONCEPT(Resolvable);