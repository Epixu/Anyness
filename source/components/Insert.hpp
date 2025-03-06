#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Defaultable.hpp>


namespace Langulus::CT
{
   template<class SELF>
   concept PackDefaultable = SELF::TypeErased or Defaultable<TypeOf<SELF>>;
   template<class SELF, class...A>
   concept PackMakableFrom = SELF::TypeErased or ::std::constructible_from<TypeOf<SELF>, A...>;
   template<class SELF, class T1, class...TN>
   concept PackInsertable = SELF::TypeErased or UnfoldMakableFrom<TypeOf<SELF>, T1, TN...>;
   template<class SELF, class A>
   concept PackAssignable = SELF::TypeErased or AssignableFrom<TypeOf<SELF>, A>;
}

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Insertion interface                                                    
   ///                                                                        
   struct Insert {
      using CTTI_Component = Yes;

      template<class SELF, class FORCE = typename SELF::DeepType, class T1, class...TN>
      auto Insert(this SELF&, CT::Index auto, T1&&, TN&&...) -> typename SELF::Count requires CT::PackInsertable<TYPE, T1, TN...>;

      template<class SELF, class FORCE = typename SELF::DeepType, class T> requires CT::Container<Deint<T>>
      auto InsertMany(this SELF&, CT::Index auto, T&&) -> typename SELF::Count;

      template<class SELF, class FORCE = typename SELF::DeepType, class T1, class...TN>
      auto Merge(this SELF&, CT::Index auto, T1&&, TN&&...) -> typename SELF::Count requires CT::PackInsertable<TYPE, T1, TN...>;

      template<class SELF, class FORCE = typename SELF::DeepType, class T> requires CT::Container<Deint<T>>
      auto MergeMany(this SELF&, CT::Index auto, T&&) -> typename SELF::Count;
   
      template<class SELF, class...A>
      decltype(auto) Emplace(this SELF&, CT::Index auto, A&&...) requires CT::PackMakableFrom<SELF, A...>;

      template<class SELF, class...A>
      auto New(this SELF&, typename SELF::Count, A&&...) -> typename SELF::Count requires CT::PackMakableFrom<SELF, A...>;

      template<class SELF>
      auto New(this SELF&, typename SELF::Count = 1) -> typename SELF::Count requires CT::PackDefaultable<SELF>;

      template<class SELF, bool CONCAT = true, class FORCE = typename SELF::DeepType>
      auto SmartPush(this SELF&, CT::Index auto, auto&&, typename SELF::StateType = {}) -> typename SELF::Count;

      template<class SELF, CT::Deep T, bool TRANSFER_OR = true>
      auto Deepen(this SELF&) -> T&;

      template<class SELF>
      void Null(this SELF&, typename SELF::Count);

      template<class SELF, class A>
      void Fill(this SELF&, A&&) requires CT::PackAssignable<SELF, A>;

      template<class SELF>
      SELF Extend(this SELF&, typename SELF::Count);
   };

} // namespace Langulus::Anyness::Component
