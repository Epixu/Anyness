#pragma once
#include "../Container.hpp"
#include <Langulus/HashOf.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access by hashing a value of the provide ID    
   /// Uses Robin Hood algorithm to manage collisions                         
   ///   @tparam ID - the stack/heap and type ID                              
   ///   @tparam HASH - type of the hash                                      
   template<unsigned ID = 0, class HASH = Hash>
   struct IndexedHash {
      using CTTI_Component = Yes;
      using CTTI_Contiguous = No;
      using TableType = uint8_t;

   protected:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      using Key = Tif<CT::Mutable<C>, typename Deref<C>::KeyMut, typename Deref<C>::Key>;

      template<CT::Container C>
      using Val = Tif<CT::Mutable<C>, typename Deref<C>::ValMut, typename Deref<C>::Val>;

      TableType* mTable = nullptr;

   public:
      template<CT::Container C>
      auto operator[] (this C&&, Key<C>) has_assumptions -> Val<C>;
   };

} // namespace Langulus::Anyness::Component
