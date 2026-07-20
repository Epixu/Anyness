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
#include <type_traits>


namespace Langulus::CT
{
   /// Check if all T are literals or bounded character arrays                
   template<class...T>
   concept TextLiteral = PartialValidate<T...> and ((
         LiteralString<T> or (Array<T> and Character<TypeOf<T>>)
      ) and ...);

   /// Check if all T are string pointers, hopefully null-terminated.         
   /// This accounts for all character pointers that <do not have extents>.   
   template<class...T>
   concept TextPointer = PartialValidate<T...>
       and ((Sparse<T> and Character<Deptr<Deref<T>>>) and ...);
   
   /// Concept for any possible standard library representation of a string.  
   /// This includes not only std::string, but also any contiguous range      
   /// that's filled with dense characters, like std::array<char> and         
   /// std::string_view.                                                      
   template<class...T>
   concept TextRange = PartialValidate<T...> and ((
         (::std::ranges::contiguous_range<T> and CT::Character<TypeOf<T>>)
         or ::std::same_as<::std::remove_cvref_t<T>, ::std::string_view>
         or ::std::same_as<::std::remove_cvref_t<T>, ::std::wstring_view>
      ) and ...);
}

namespace Langulus::CTTI
{
   /// Affects CT::Text<T>                                                    
   template<class T>
   struct Text {
      static constexpr bool Default = true;
      static constexpr bool Enabled = CT::TextLiteral<T>
                                   or CT::TextPointer<T>
                                   or CT::TextRange<T>;
   };
}

LANGULUS_CTTI_CONCEPT_DECVQ(Text);
