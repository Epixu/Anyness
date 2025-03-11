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
      using CTTI_Typed     = TYPE;

      /// Get the type definition                                             
      ///   @return the definition                                            
      T GetType() const noexcept {
         return MetaOf<TYPE>();
      }

      bool Is(T type) const noexcept {
         return GetType().Is(type);
      }

      template<CT::NotVoid T1, CT::NotVoid...TN>
      consteval bool Is() const noexcept {
         return CT::SameAsOneOf<TYPE, T1, TN...>;
      }

      bool IsSimilar(T type) const noexcept {
         return GetType().IsSimilar(type);
      }

      template<CT::NotVoid T1, CT::NotVoid...TN>
      consteval bool IsSimilar() const noexcept {
         return CT::SimilarAsOneOf<TYPE, T1, TN...>;
      }

      /// Check if this type is exactly like another                          
      ///   @param type - the type to match                                   
      ///   @return true if data type matches type exactly                    
      bool IsExact(T type) const noexcept {
         return GetType().IsExact(type);
      }

      /// Check if this type is exactly one of the provided types             
      ///   @tparam T1, TN... - the types to compare against                  
      ///   @return true if data type matches at least one type               
      template<CT::NotVoid T1, CT::NotVoid...TN>
      consteval bool IsExact() const {
         return CT::ExactAsOneOf<TYPE, T1, TN...>;
      }
   };

} // namespace Langulus::Anyness::Component
