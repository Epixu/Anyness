#pragma once
#include "../Container.hpp"
#include "Indexed-Linear.hpp"


namespace Langulus::Anyness::Component
{
   
   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible         
   ///   @attention type-erased elements are always emplaceable, because      
   ///      all arguments will be encapsulated in a descriptor, and will fail 
   ///      at runtime if not reflected as descriptor-constructible           
   template<class C, class...A>
   concept RangeEmplaceable = CT::Container<C> and (
      C::TypeErased or ::std::constructible_from<TypeOf<C>, A...>
   );


   ///                                                                        
   /// Implements emplacement for containers                                  
   ///   @tparam ID - heap we're inserting to                                 
   template<unsigned ID = 0>
   struct Emplacement {
      using CTTI_Component = Yes;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't emplace stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using PickMut = typename C::PickMut;

   public:
      /// Emplacement at specific index                                       
      template<CT::Container C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> PickMut<C> requires (C::Indexed and RangeEmplaceable<C, A...>);

      /// Generic emplacement                                                 
      template<CT::Container C, class...A>
      auto Emplace(this C&, A&&...)
         -> PickMut<C> requires RangeEmplaceable<C, A...>;
   };

} // namespace Langulus::Anyness::Component
