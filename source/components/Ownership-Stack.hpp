///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Ownership-Emergent.hpp"


namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.OwnershipStack<STYLE, ID, SHARED...>

   ///                                                                        
   /// Keep a pointer to the heap allocation as a member.                     
   /// Manage its ownership by referencing and dereferencing it.              
   ///   @tparam STYLE whether ownership will be automatically applied on     
   ///      construction, reassignment and destruction. Usually 0 if container
   ///      is just a view, or in other cases where you want to carry an      
   ///      allocation pointer, but not necessarily reference it.             
   ///   @tparam ID provider we're keeping track of                           
   ///   @tparam SHARED other providers that will share the same allocation   
   ///      variable.                                                         
   template<uint STYLE, Cid ID, Cid...SHARED>
   struct OwnershipStack : OwnershipEmergent<STYLE, ID, SHARED...> {
      using StackRequest = AllocationPtr;

      template<Cid SID>
      static constexpr bool Relevant = IdMatch<SID, ID, SHARED...>;

      /// Get the allocation                                                  
      template<Cid SID = ID> requires Relevant<SID>
      constexpr auto GetAllocation(this auto const& self) noexcept {
         return ThisCom::GetAllocationInner();
      }

      /// Shallow-copy all initialized elements in memory to another          
      /// allocation, that is owned once only by this container.              
      ///   @attention if we already own the memory just Keep() it once       
      template<Cid SID = ID, CT::Container C>
      requires (CT::HeapAllocated<C> and Relevant<SID>)
      void TakeOwnership(this C& self) {
         auto rawData = self.template GetRaw<SID>();
         if (not rawData)
            return;

         auto& a = ThisCom::GetAllocationInner();
         if (a)
            return; // We already own this allocation                   

      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         // The heap might already be ours and we just don't know it    
         if (auto found = Allocator::Find(rawData)) {
            a = found;
            DecvqAllCast(a)->AddRef(1);
            return;
         }
      #endif

         // Shallow-copy all elements in a fresh allocation             
         C temp {Copy {self}};
         self = Abandon {temp};
      }

   protected:
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComEmplacement(friend);

      /// Get allocation (inner)                                              
      ///   @attention may be uninitialized                                   
      template<Cid SID = ID> requires Relevant<SID>
      constexpr auto& GetAllocationInner(this auto&& self) noexcept {
         return self.template AccessStack<OwnershipStack>();
      }
      
      /// Set allocation (inner)                                              
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = ID> requires Relevant<SID>
      constexpr void SetAllocationInner(this auto& self, Allocation const* a) noexcept {
         ThisCom::GetAllocationInner() = const_cast<Allocation*>(a);
      }

      /// Automatically set the allocation by searching for it using the heap 
      /// pointer. If allocation wasn't found, it will be set to nullptr.     
      ///   @attention this will not dereference previous allocation          
      template<Cid SID = ID> requires Relevant<SID>
      void FindAllocationInner(this auto& self) noexcept {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if (auto found = Allocator::Find(self.template GetRaw<SID>())) {
               ThisCom::SetAllocationInner(found);
               if constexpr (STYLE & OnCreate)
                  ThisCom::Keep();
            }
            else
         #endif
         ThisCom::SetAllocationInner(nullptr);
      }

      /// Resets allocation and all of its derivatives                        
      template<Cid SID = ID> requires Relevant<SID>
      constexpr void ResetAllocationInner(this auto&& self) noexcept {
         ThisCom::SetAllocationInner(nullptr);
         if_available(self.template SetReservedInner<SID>(0));
         if_available(self.template SetHashTableInner<SID>(nullptr));
         self.template ResetCount<SID>();
      }

      /// Default-initialize the component                                    
      ///   @attention this will not dereference previous allocation          
      constexpr void ConstructDefault(this auto& self) noexcept {
         ThisCom::SetAllocationInner(nullptr);
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @attention this will not dereference previous allocation          
      ///   @param self deduced this                                          
      ///   @param intent the intent and container to transfer from           
      ///   @important notice that Copy and Clone intents are not handled     
      ///      here. They're handled in heap components instead, in case      
      ///      something throws an exception while constructing.              
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         if constexpr (not CT::Copied<I> and not CT::Cloned<I>) {
            decltype(auto) from = LglsFwd(intent.what);

            if constexpr (CT::Referred<I>) {
               // Refer                                                 
               if constexpr (requires { from.GetAllocationInner(); }) {
                  ThisCom::SetAllocationInner(from.GetAllocationInner());
                  if constexpr (STYLE & OnCreate)
                     ThisCom::Keep();
               }
               else ThisCom::FindAllocationInner();
            }
            else if constexpr (CT::Abandoned<I> or CT::Moved<I>) {
               // Abandon/Move                                          
               if constexpr (requires { from.GetAllocationInner(); }) {
                  ThisCom::SetAllocationInner(from.GetAllocationInner());

                  if_available(from.SetAllocationInner(nullptr))
                  else if constexpr (STYLE & OnCreate and CT::StronglyOwned<I>) {
                     // We can't reset source allocation pointer, which 
                     // means that source destructor will dereference   
                     // when out of scope: must reference data here.    
                     ThisCom::Keep();
                  }
               }
               else ThisCom::FindAllocationInner();
            }
            else if constexpr (CT::Disowned<I>) {
               // Disown                                                
               ThisCom::SetAllocationInner(nullptr);
            }
         }
      }
   };

   #undef ThisCom
}
