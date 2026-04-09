///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/Signed.hpp>
#include <Langulus/CT/Integer.hpp>


namespace Langulus::Anyness::Component
{

   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.CountStatic<ID, COUNT, SHARED...>

   ///                                                                        
   ///   A compile-time count                                                 
   ///                                                                        
   ///   Count shows how many elements inside a container are initialized.    
   ///   Compile-time counting isn't really counting, and doesn't take up     
   /// space, but is useful for defining single-element containers that       
   /// still need the API required to function alongside other components.    
   ///   In these cases, count is equal to COUNT if container has a heap      
   /// component that has been allocated - otherwise it is 0. If no heap      
   /// component exists or can't be null, then the count is always COUNT.     
   ///   @tparam ID provider ID to keep count of                              
   ///   @tparam COUNT the count type and value                               
   ///   @tparam SHARED provider IDs that share the same count variable       
   template<Cid ID, auto COUNT, Cid...SHARED>
   struct CountStatic {
      using CTTI_Component  = Yes<>;
      using CTTI_Contiguous = Maybe<COUNT == 1>;

      static constexpr Cid  Id = ID;
      static constexpr int  ComponentPrecedence = -1000;
      static constexpr bool ContainsMany = COUNT > 1;

      using CountType   = decltype(COUNT);
      using ReserveType = CountType;
      using IndexType   = Index::At<CountType>;

      static_assert(COUNT > 0,
         "Can't have a container of zero or negative count");
      static_assert(CT::Integer<CountType> and not CT::Signed<CountType>,
         "Count type must be an unsigned integer");

      /// Equal to COUNT if container has a heap component that has been      
      /// allocated - zero otherwise. If no heap component exists, then the   
      /// count is always COUNT.                                              
      template<Cid SID = ID>
      constexpr auto GetCount(this auto const& self) noexcept -> CountType {
         static_assert(SID == ID or ((SID == SHARED) or ...),
            "Wrong count instance");
         return ThisCom::template GetCountInner<SID>();
      }
      
      /// Check if empty                                                      
      template<Cid SID = ID>
      constexpr bool IsEmpty(this auto const& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...),
            "Wrong count instance");
         return ThisCom::template GetCountInner<SID>() == CountType {0};
      }

      /// Explicit boolean conversion to allow using containers in ifs        
      explicit constexpr operator bool(this auto const& self) noexcept {
         return ThisCom::GetCountInner() != CountType {0};
      }

   protected:
      template<Cid, Cid...>         friend struct Removal;
      template<Cid, Cid...>         friend struct Emplacement;
      template<Cid, class>          friend struct Insertion;
      template<Cid, Cid...>         friend struct IndexedLinear;
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, Cid...>         friend struct Conversion;

      /// Get count (inner)                                                   
      template<Cid SID = ID, CT::Container C>
      constexpr auto GetCountInner(this C const& self) noexcept -> CountType {
         static_assert(SID == ID or ((SID == SHARED) or ...),
            "Wrong count instance");

         if constexpr (CT::HasVariableCount<C>)
            return self.template GetHeapInner<SID>() ? COUNT : CountType {0};
         else
            return COUNT;
      }

      /// Reset count (inner)                                                 
      ///   @attention doesn't destroy elements, only resets hash and count   
      template<Cid SID = ID, CT::Container C>
      constexpr void ResetCount(this C& self) noexcept {
         static_assert(SID == ID or ((SID == SHARED) or ...),
            "Wrong count instance");

         if_available(self.template SetHeapInner<SID>(nullptr));
         if_available(self.template SetHashInner<SID>(1));
      }
   };

   #undef ThisCom
}
