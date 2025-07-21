///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"
#include "../Literal.hpp"


namespace Langulus::CTTI
{
   /// Used to define a custom file extensions for serialization.             
   /// Can be used in two ways to satisfy CT::Files<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Files = Yes<"txt,rtf,etc">;` in T          
   template<class T>
   struct Files {
      static constexpr Literal Name = "<missing file extensions>";
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Files);

namespace Langulus
{
   /// Get the file extensions of a type at compile-time                      
   ///   @tparam T - the type to get the file extensions of                   
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto FilesOf() {
      using DT = Decvq<Deref<T>>;
      static_assert(CT::Void<T> or CT::Complete<T>,
         "Can't get file extensions of an incomplete type");
      
      if constexpr (CTTI::Files<DT>::Enabled) {
         constexpr auto s = CTTI::Files<DT>::Name;
         static_assert(IsASCII(s), "File extensions must be ASCII");
         return s;
      }
      else if constexpr (::std::is_class_v<DT>) {
         if constexpr (requires { DT::CTTI_Files::Constant; }) {
            constexpr auto s = DT::CTTI_Files::Constant;
            static_assert(IsASCII(s), "File extensions must be ASCII");
            return DT::CTTI_Files::Constant;
         }
         else return Literal {};
      }
      else return Literal {};
   }
}
