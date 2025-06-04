///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "CT/DefineTag.hpp"


namespace Langulus::Inner
{

   ///                                                                        
   ///   Tags are types that give additional context to data without          
   /// changing its behavior                                                  
   ///   They can be reflected and serialized, and aid in selecting variables,
   /// describing objects, etc. You can think of them as if variable names    
   /// weren't infinite, but rather a finite set of general usage patterns.   
   ///   For example, a variable 'count' can appear in many objects and       
   /// containers, but it always does essentially the same thing - it keeps   
   /// track of a number of things. You can encode that semantic meaning by   
   /// tagging all these variables with Tags::Count<int> mSomeVariable;       
   /// across your code. This can be leveraged when reflecting members, later 
   /// used in descriptor-construction, when seeking data, and more.          
   ///   @tparam T - the data behind the tag                                  
   ///                                                                        
   template<CT::NotVoid T>
   struct Tag;

   /// Since we can't inherit from fundamental types or references/pointers,  
   /// we have to wrap them inside the tag                                    
   template<CT::NotVoid T> requires (CT::NotDecayed<T> or CT::Fundamental<T>)
   struct Tag<T> {
      static constexpr bool CTTI_Tag = true;
      T value;
   };
   
   /// We can inherit from all the rest                                       
   template<CT::NotVoid T> requires (CT::Decayed<T> and CT::NotFundamental<T>)
   struct Tag<T> : T {
      static constexpr bool CTTI_Tag = true;
      using T::T;
   };

} // namespace Langulus::Inner

namespace Langulus::Anyness
{

   /// A type-erased dynamic tag, that depends on Anyness::Many               
   /// If incomplete, include <Langulus/Anyness/Tag.hpp>                      
   struct Tag;

   /// A statically typed dynamic tag                                         
   template<CT::NotVoid>
   struct TTag;

} // namespace Langulus::Anyness

namespace Langulus::CT
{

   template<class...T>
   concept Tag = (T::CTTI_Tag and ...);
   template<class...T>
   concept NotTag = ((not Tag<T>) and ...);

} // namespace Langulus::CT