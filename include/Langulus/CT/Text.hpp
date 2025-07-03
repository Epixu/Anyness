///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Character.hpp"
#include "../TypeOf.hpp"
#include <ranges>


namespace Langulus::CT
{

   /// Check if all T are literals or bounded character arrays                
   template<class...T>
   concept TextLiteral = ((
         LiteralString<T> or (Array<T> and Character<TypeOf<T>>)
      ) and ...);

   /// Check if all T are string pointers, hopefully null-terminated          
   /// This accounts for all character pointers that <do not have extents>    
   template<class...T>
   concept TextPointer = ((Sparse<T> and Character<Deptr<Deref<T>>>) and ...);
   
   /// Concept for any possible standard library representation of a string   
   /// This includes not only std::string, but also any contiguous range      
   /// that's filled with dense characters                                    
   template<class...T>
   concept TextRange = ((::std::ranges::contiguous_range<T>
       and CT::Character<TypeOf<T>>
      ) and ...);

} // namespace Langulus::CT

namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Text<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Text = Yes;` in T                          
   template<class T>
   struct Text {
      static constexpr bool Enabled = CT::TextLiteral<T>
                                   or CT::TextPointer<T>
                                   or CT::TextRange<T>;
   };
   
} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Text);
