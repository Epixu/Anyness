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
   template<class T>
   struct Tag;

   /// Since we can't inherit from fundamental types, we have to wrap them    
   /// inside the tag                                                         
   template<CT::Fundamental T>
   struct Tag<T> {
      using CTTI_Typed = T;
      T value;
   };
   
   /// We can inherit from all the rest                                       
   template<CT::NotFundamental T>
   struct Tag<T> : T {
      using CTTI_Typed = T;
   };

} // namespace Langulus::Inner

namespace Langulus::Tags
{

   template<class T>
   struct Name : Inner::Tag<T> {
      using CTTI_DefineTag = YesText<"Name">;
   };

   template<class T>
   struct Count : Inner::Tag<T> {
      using CTTI_DefineTag = YesText<"Count">;
   };

} // namespace Langulus::Tags

namespace Langulus::CT
{

   template<class...T>
   concept Tag = DefineTag<T...>;
   template<class...T>
   concept NotTag = NotDefineTag<T...>;

} // namespace Langulus::CT
