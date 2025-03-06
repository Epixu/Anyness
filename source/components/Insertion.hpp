#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Defaultable.hpp>
#include <Langulus/CT/Unfold.hpp>


namespace Langulus::CT
{

   /// Check if a T is constructible with each of the provided arguments,     
   /// either directly or by being unfolded                                   
   template<class T, class...A>
   concept UnfoldConstructible = ((::std::constructible_from<T, A>
                                or ::std::constructible_from<T, Unfold<A>>) and ...);

   /// Check if a T is assignable with each of the provided arguments,        
   /// either directly or by being unfolded                                   
   template<class T, class...A>
   concept UnfoldAssignable = ((::std::assignable_from<T&, A>
                             or ::std::assignable_from<T&, Unfold<A>>) and ...);

} // namespace Langulus::CT

namespace Langulus::Anyness::Component
{
   
   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible         
   ///   @attention type-erased elements are always emplaceable, because      
   ///      all arguments will be encapsulated in a descriptor, and will fail 
   ///      at runtime if not reflected as descriptor-constructible           
   template<class SELF, class...A>
   concept RangeEmplaceable = SELF::TypeErased
                         or ::std::constructible_from<TypeOf<SELF>, A...>;

   /// Check if container's elements are unfold-constructible                 
   ///   @attention type-erased elements are always insertable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class SELF, class T1, class...TN>
   concept RangeInsertable = SELF::TypeErased
                         or CT::UnfoldConstructible<TypeOf<SELF>, T1, TN...>;

   /// Check if container's elements are unfold-assignable                    
   ///   @attention type-erased elements are always assignable, and will fail 
   ///      at runtime if not reflected as such                               
   template<class SELF, class A>
   concept RangeAssignable = SELF::TypeErased
                         or CT::UnfoldAssignable<TypeOf<SELF>, A>;


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
      using RangeView = typename C::RangeViewType;
      template<CT::Container C>
      using ItemView  = typename C::ItemViewType;

   public:
      /// Insertion at specific index                                         
      template<CT::Container C, class FORCE = Deep<C>, class A1, class...AN>
      auto InsertAt(this C&, CT::Index auto, A1&&, AN&&...)
         -> Count<C> requires (C::Indexed and RangeInsertable<C, A1, AN...>);

      template<CT::Container C, class FORCE = Deep<C>>
      auto InsertRangeAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C> requires C::Indexed;

      template<CT::Container C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> ItemView<C> requires (C::Indexed and RangeEmplaceable<C, A...>);

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

      template<CT::Container C, class...A>
      auto Emplace(this C&, A&&...)
         -> ItemView<C> requires RangeEmplaceable<C, A...>;

      template<CT::Container C, bool CONCAT = true, class FORCE = Deep<C>>
      auto SmartPush(this C&, auto&&, State<C> = {})
         -> Count<C>;

      template<CT::Container C, bool TRANSFER_OR = true>
      auto Deepen(this C&) -> Deep<C>&;

      template<CT::Container C>
      void Null(this C&, Count<C>);

      template<CT::Container C, class A>
      void Fill(this C&, A&&) requires RangeAssignable<C, A>;

      template<CT::Container C, class...A>
      auto Extend(this C&, Count<C> = 1, A&&...) -> RangeView<C>;
   };

} // namespace Langulus::Anyness::Component
