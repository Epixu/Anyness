#pragma once
#include <Langulus/MetaOf.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Defines the contained type at compile-time                             
   /// Doesn't allow for type-erasure and doesn't take up space               
   ///   @tparam T    - the type of the variable                              
   ///   @tparam TYPE - static type, can't be void                            
   ///   @tparam ID   - which heap/stack is typed?                            
   template<class T, CT::NotVoid TYPE, unsigned ID = 0>
   struct TypedStatic {
      using CTTI_Component = Yes;
      using CTTI_Typed = TYPE;

      T GetType() const noexcept { return MetaOf<TYPE>(); }
   };

} // namespace Langulus::Anyness::Component
