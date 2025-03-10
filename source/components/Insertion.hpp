#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Unfold.hpp>


namespace Langulus::Anyness::Component
{
   
   /// Check if container's elements are unfold-constructible                 
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeInsertable = CT::Container<C> and (
      C::TypeErased or CT::UnfoldConstructible<TypeOf<C>, T1, TN...>
   );


   ///                                                                        
   /// Implements insertion for containers                                    
   ///                                                                        
   struct Insertion {
      using CTTI_Component = Yes;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't insert stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep  = typename C::DeepType;
      template<CT::Container C>
      using State = typename C::StateType;
      template<CT::Container C>
      using PickRangeMut = typename C::PickRangeMut;

   public:
      /// Insertion at specific index                                         
      template<CT::Container C, class FORCE = Deep<C>, class A1, class...AN>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires (C::Indexed and RangeInsertable<C, A1, AN...>);

      template<CT::Container C, class FORCE = Deep<C>>
      auto InsertRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C> requires C::Indexed;

      template<CT::Container C, bool CONCAT = true, class FORCE = Deep<C>>
      auto SmartPushAt(this C&, CT::Index auto, auto&&, State<C> = {})
         -> Count<C> requires C::Indexed;

      /// Generic insertion                                                   
      template<CT::Container C, class FORCE = Deep<C>, class A1, class...AN>
      auto Insert(this C&, A1&&, AN&&...)
         -> Count<C> requires RangeInsertable<C, A1, AN...>;

      template<CT::Container C, class FORCE = Deep<C>>
      auto InsertRange(this C&, CT::Container auto&&)
         -> Count<C>;

      template<CT::Container C, bool CONCAT = true, class FORCE = Deep<C>>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<CT::Container C, bool TRANSFER_OR = true>
      auto Deepen(this C&) -> Deep<C>&;

      template<CT::Container C>
      void Null(this C&, Count<C>);

      template<CT::Container C, class...A>
      auto Extend(this C&, Count<C> = 1, A&&...) -> PickRangeMut<C>;
   };

} // namespace Langulus::Anyness::Component
