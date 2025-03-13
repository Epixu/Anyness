#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Unfold.hpp>
#include <Langulus/CT/Index.hpp>


namespace Langulus::Anyness
{
   
   /// Check if container's elements are unfold-constructible                 
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class C, class T1, class...TN>
   concept RangeInsertable = CT::Container<C> and (
      C::TypeErased or CT::UnfoldConstructible<TypeOf<C>, T1, TN...>
   );

} // namespace Langulus::Anyness

namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Implements insertion for containers                                    
   ///   @tparam AS - type to serialize as before inserting. Useful for byte  
   ///      and text containers. Use void to insert without serialization     
   template<class AS = void>
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
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires (C::Indexed and RangeInsertable<C, A1, AN...>);

      template<bool CONCAT = true, bool FORCE = true, CT::Container C>
      auto SmartPushAt(this C&, CT::Index auto, auto&&, State<C> = {})
         -> Count<C> requires C::Indexed;

      /// Generic insertion                                                   
      template<bool FORCE = true, class A1, class...AN, CT::Container C>
      auto Insert(this C&, A1&&, AN&&...)
         -> Count<C> requires RangeInsertable<C, A1, AN...>;

      template<bool CONCAT = true, bool FORCE = true, CT::Container C>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<bool TRANSFER_OR = true, CT::Container C>
      auto Deepen(this C&) -> Deep<C>&;

      template<CT::Container C>
      void Null(this C&, Count<C>);

      template<CT::Container C, class...A>
      auto Extend(this C&, Count<C> = 1, A&&...) -> PickRangeMut<C>;
   };

} // namespace Langulus::Anyness::Component
