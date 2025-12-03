///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Typenav.hpp"
#include "CT/DefineTag.hpp"


namespace Langulus
{
   ///                                                                        
   ///   Tags are types that give additional context to data without          
   /// changing its behavior.                                                 
   ///   They can be reflected and serialized, and aid in selecting variables,
   /// describing objects, etc. You can think of them as if variable names    
   /// weren't infinite, but rather a finite set of general usage patterns.   
   ///   For example, a variable 'count' can appear in many objects and       
   /// containers, but it always does essentially the same thing - it keeps   
   /// track of a number of things. You can encode that semantic meaning by   
   /// tagging all these variables in two ways:                               
   ///   1) Tags::Count<int> mSomeVariable;                                   
   ///   2) Tag<int, Tags::Count, Tags::Other> mSomeVariable;                 
   /// This can be leveraged when reflecting members, later used in           
   /// descriptor-construction, when seeking data, and more.                  
   ///   @tparam T - the data behind the tag                                  
   ///   @tparam TAGS - the tags                                              
   template<class T, class...TAGS>
   struct Tag;

   /// Since we can't inherit from fundamental types or references/pointers,  
   /// we have to wrap them inside the tag                                    
   template<CT::NotVoid T, CT::DefineTag...TAGS>
   requires (CT::NotDecayed<T> or CT::Fundamental<T>)
   struct Tag<T, TAGS...> {
      static_assert(sizeof...(TAGS) > 0, "No tags specified");
      using CTTI_ReflectAs = T;
      using CTTI_Tags = Types<TAGS...>;
      T value;
   };
   
   /// We can inherit from all the rest                                       
   template<CT::NotVoid T, CT::DefineTag...TAGS>
   requires (CT::Decayed<T> and CT::NotFundamental<T>)
   struct Tag<T, TAGS...> : T {
      static_assert(sizeof...(TAGS) > 0, "No tags specified");
      using CTTI_ReflectAs = T;
      using CTTI_Tags = Types<TAGS...>;
      using T::T;
   };
}
