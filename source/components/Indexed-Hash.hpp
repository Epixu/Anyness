#pragma once
#include "../Container.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access by hashing a value of type T            
   ///   @tparam T - type to get hash of, use void for type-erasure           
   ///   @tparam HASH - type of the hash                                      
   template<class HASH = Hash>
   struct IndexedHash {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      using Key = Tif<CT::Mutable<C>, typename Deref<C>::KeyMut, typename Deref<C>::Key>;

      template<CT::Container C>
      using Val = Tif<CT::Mutable<C>, typename Deref<C>::ValMut, typename Deref<C>::Val>;

   public:
      template<CT::Container C>
      auto operator[] (this C&&, Key<C>) has_assumptions -> Val<C>;
   };

} // namespace Langulus::Anyness::Component
