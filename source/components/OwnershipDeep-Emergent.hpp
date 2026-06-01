///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/Assume.hpp>
#include <Langulus/Allocator.hpp>
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>


namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
}

namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.OwnershipDeepEmergent<REF_INDIVIDUAL, ID, SHARED...>

   ///                                                                        
   /// Manages deep ownership by searching for an allocation every time.      
   /// Also used as base for other deep ownership components.                 
   ///   @tparam REF_INDIVIDUAL toggles whether contained items that were     
   ///      reflected as CT::Referenced get referenced. Elements will get     
   ///      referenced even if no entry for the element exist, but you can    
   ///      avoid referencing altogether if you use the Disown intent.        
   ///      To be more specific - when GetAllocation() is nullptr and the     
   ///      entire container is considered disowned.                          
   ///   @tparam ID which heap/stack are we keeping track of?                 
   ///   @tparam SHARED additional provider IDs that share the same behavior  
   template<bool REF_INDIVIDUAL, Cid ID, Cid...SHARED>
   struct OwnershipDeepEmergent {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr bool DeeplyOwned = true;
      static constexpr bool ReferenceElements = REF_INDIVIDUAL;
      static constexpr int  ComponentPrecedence = 2000;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

   protected:
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComRemoval(friend);
      LglsComEmplacement(friend);
      LglsComOwnershipEmergent(friend);

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Find _and populate_ an indirection entry                            
      template<bool CUSTOM_POINTERS, CT::Container C>
      static void FindEntry(
         AllocationPtr const* entries, CT::Sparse auto ptr,
         [[maybe_unused]] const DMeta& T = {},
         [[maybe_unused]] const DMeta& nextT = {}
      ) noexcept {
         static_assert(LANGULUS_FEATURE(MANAGED_MEMORY));
         if (*entries)
            return;

         // We can find the allocation behind the pointer but in order  
         // to save it, we must make sure that entry array resides in   
         // non-shared memory (when OwnershipDeepHeap is used)          
         auto entry_allocation = Allocator::Find(entries);
         if (not entry_allocation or entry_allocation->GetUses() != 1)
            return;

         auto& entry = DecvqAllCast(*entries);
         if constexpr (CT::TypeErased<C>) {
            if constexpr (CUSTOM_POINTERS) {
               const auto ptrSpec = T.GetPointerSpecification();
               if (ptrSpec.IsPacked()) {
                  uintptr_t derefptr = 0;
                  memcpy(&derefptr, ptr, ptrSpec.GetTotalBytes());
                  entry = DecvqAllCast(Allocator::FindPackedPointer(
                     ptrSpec, nextT, derefptr
                  ));
               }
               else entry = DecvqAllCast(Allocator::Find(*static_cast<void**>(ptr)));
            }
            else entry = DecvqAllCast(Allocator::Find(ptr));
         }
         else entry = DecvqAllCast(Allocator::Find(ptr));
      }
   #endif

      /// Nests through all indirection layers of the first contained element.
      /// Relies on predeclared array of GetEntriesInner (i.e. not emergent). 
      ///   @tparam FIND_MISSING if an entry is missing, we attempt at finding
      ///      it in the memory manager, if MANAGED_MEMORY feature is enabled 
      ///   @attention individuals will be referenced if REF_INDIVIDUAL is    
      ///      enabled, regardless if an entry was found.                     
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<bool FIND_MISSING = false, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void KeepElementDeepStandardPointers(this C& self) assumptious {
         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.template GetAllocation<SID>(),
               "Can't keep anything in a container without ownership");
         }

         auto entries = self.template GetEntriesInner<SID>();
         if (not entries)
            return;

         using H = Decay<decltype(LglsFake(DecideHandle<C>).template PickDimension<SID>())>;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Referencing a type-erased element                        
            const auto T = self.template GetType<SID>();
            LglsAssumeDev(T.IsSparse(),
               "Don't call KeepElementDeepStandardPointers if container isn't sparse");

            const auto subT = T.GetDeptr();
            const auto ptr = *static_cast<void**>(self.template GetRawVoid<SID>());
            LglsAssumeDevAndOptimize(ptr, "Null pointer");

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               if constexpr (FIND_MISSING)
                  ThisCom::template FindEntry<false, C>(entries, ptr);
            #endif

            if (subT.IsSparse()) {
               // Pointer to pointer                                    
               H temp {ptr, entries + 1, subT};
               temp.template KeepElementDeepStandardPointers<FIND_MISSING>();
            }
            else if constexpr (REF_INDIVIDUAL) {
               if (const auto referencer = subT.GetReferencer()) {
                  // Pointer to dense                                   
                  referencer(ptr, 1);
               }
            }
         }
         else {
            //                                                          
            // Referencing a statically-typed element                   
            using T = TypeOf<C, SID>;         
            static_assert(CT::Sparse<T>,
               "Don't call KeepElementDeepStandardPointers if container isn't sparse");

            using DT = Deptr<T>;
            auto ptr = *self.template GetRaw<SID>();
            LglsAssumeDevAndOptimize(ptr, "Null pointer");

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               if constexpr (FIND_MISSING)
                  ThisCom::template FindEntry<false, C>(entries, ptr);
            #endif

            if constexpr (CT::Sparse<DT>) {
               // Pointer to pointer                                    
               typename H::Denser temp {ptr, entries + 1};
               temp.template KeepElementDeepStandardPointers<FIND_MISSING>();
            }
            else if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
               // Pointer to dense                                      
               ptr->Reference(1);
            }
         }

         if (*entries)
            DecvqAllCast(*entries)->AddRef(1);
      }

      /// Nests through all indirection layers of the first contained element.
      /// Emergent - every indirection will be sought in the memory manager   
      /// if MANAGED_MEMORY is enabled.                                       
      ///   @attention individuals will be referenced if REF_INDIVIDUAL is    
      ///      enabled, regardless if an entry was found.                     
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      void KeepElementDeepStandardPointersEmergent(this C& self) assumptious {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            Allocation const* entry = nullptr;
         #endif

         using H = Decay<decltype(LglsFake(DecideHandle<C>).template PickDimension<SID>())>;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Referencing a type-erased element                        
            const auto T = self.template GetType<SID>();
            LglsAssumeDev(T.IsSparse(),
               "Don't call KeepElementDeepStandardPointersEmergent if container isn't sparse");

            const auto subT = T.GetDeptr();
            const auto ptr = *static_cast<void**>(self.template GetRawVoid<SID>());
            LglsAssumeDevAndOptimize(ptr, "Null pointer");

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               entry = Allocator::Find(ptr);
            #endif

            if (subT.IsSparse()) {
               // Pointer to pointer                                    
               H temp {ptr, nullptr, subT};
               temp.KeepElementDeepStandardPointersEmergent();
            }
            else if constexpr (REF_INDIVIDUAL) {
               if (const auto referencer = subT.GetReferencer()) {
                  // Pointer to dense                                   
                  referencer(ptr, 1);
               }
            }

         }
         else {
            //                                                          
            // Referencing a statically-typed element                   
            using T = TypeOf<C, SID>;         
            static_assert(CT::Sparse<T>,
               "Don't call KeepElementDeepStandardPointersEmergent if container isn't sparse");

            using DT = Deptr<T>;
            auto ptr = *self.template GetRaw<SID>();
            LglsAssumeDevAndOptimize(ptr, "Null pointer");

            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               entry = Allocator::Find(ptr);
            #endif

            if constexpr (CT::Sparse<DT>) {
               // Pointer to pointer                                    
               typename H::Denser temp {ptr, nullptr};
               temp.KeepElementDeepStandardPointersEmergent();
            }
            else if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
               // Pointer to dense                                      
               ptr->Reference(1);
            }
         }

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            if (entry)
               DecvqAllCast(entry)->AddRef(1);
         #endif
      }

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Nests through all indirection layers of the first contained element.
      /// Supports any number of custom pointer indirections.                 
      /// Relies on predeclared array of GetEntriesInner (i.e. not emergent). 
      ///   @tparam FIND_MISSING if an entry is missing, we attempt at finding
      ///      it in the memory manager, if MANAGED_MEMORY feature is enabled 
      ///   @attention individuals will be referenced if REF_INDIVIDUAL is    
      ///      enabled, regardless if an entry was found.                     
      ///   @attention assumes container is not disowned!                     
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<bool FIND_MISSING = false, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void KeepElementDeepCustomPointers(this C& self) assumptious {
         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.template GetAllocation<SID>(),
               "Can't keep anything in a container without ownership");
         }

         // Check if disowned/outside authority                         
         auto entries = self.template GetEntriesInner<SID>();
         if (not entries)
            return;

         if constexpr (CT::TypeErased<C>) {
            // Check if containing indirections                         
            auto T = self.template GetType<SID>();
            LglsAssumeDev(T.IsSparse(),
               "Don't call KeepElementDeepCustomPointers if container isn't sparse");

            void* src = self.template GetRawVoid<SID>();
            while (src and T.IsSparse()) {
               auto nextT = T.GetDeptr();
               if constexpr (FIND_MISSING)
                  ThisCom::template FindEntry<true, C>(entries, src, T, nextT);

               if (nextT.IsSparse()) {
                  // Pointer T -> Pointer nextT                         
                  T.GetDereffer()(src, &src);
               }
               else if constexpr (REF_INDIVIDUAL) {
                  if (const auto referencer = nextT.GetReferencer()) {
                     // Pointer T -> Dense nextT                        
                     referencer(const_cast<void*>(UnpackPointer(T, nextT, src)), 1);
                  }
               }

               if (*entries)
                  DecvqAllCast(*entries)->AddRef(1);

               // Move to next indirection                              
               T = nextT;
               ++entries;
            }
         }
         else {
            using T = TypeOf<C, SID>;
            static_assert(CT::Sparse<T>,
               "Don't call KeepElementDeepCustomPointers if container isn't sparse");

            auto& ptr = *self.template Get<void, SID>();
            ForEachIndirection(ptr, [&](auto& i) {
               if constexpr (FIND_MISSING)
                  ThisCom::template FindEntry<true, C>(entries, i);

               if (*entries)
                  DecvqAllCast(*entries)->AddRef(1);

               ++entries;
            });

            if constexpr (REF_INDIVIDUAL and CT::Referenced<Decay<T>>)
               DenseCast(ptr).Reference(1);
         }
      }

      /// Nests through all indirection layers of the first contained element.
      /// Supports any number of custom pointer indirections.                 
      /// Emergent - every indirection will be sought in the memory manager.  
      ///   @attention individuals will be referenced if REF_INDIVIDUAL is    
      ///      enabled, regardless if an entry was found.                     
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<Cid SID = ID, CT::Container C> requires Relevant<SID>
      void KeepElementDeepCustomPointersEmergent(this C& self) assumptious {
         if constexpr (CT::TypeErased<C>) {
            // Check if containing indirections                         
            auto T = self.template GetType<SID>();
            LglsAssumeDev(T.IsSparse(),
               "Don't call KeepElementDeepCustomPointersEmergent if container isn't sparse");

            void* src = self.template GetRawVoid<SID>();
            while (src and T.IsSparse()) {
               auto nextT = T.GetDeptr();
               auto entry = Allocator::Find(src);

               if (nextT.IsSparse()) {
                  // Pointer T -> Pointer nextT                         
                  T.GetDereffer()(src, &src);
               }
               else if constexpr (REF_INDIVIDUAL) {
                  if (const auto referencer = nextT.GetReferencer()) {
                     // Pointer T -> Dense nextT                        
                     referencer(const_cast<void*>(UnpackPointer(T, nextT, src)), 1);
                  }
               }

               if (entry)
                  DecvqAllCast(entry)->AddRef(1);

               // Move to next indirection                              
               T = nextT;
            }
         }
         else {
            using T = TypeOf<C, SID>;
            static_assert(CT::Sparse<T>,
               "Don't call KeepElementDeepCustomPointersEmergent if container isn't sparse");

            auto& ptr = *self.template Get<void, SID>();
            ForEachIndirection(ptr, [&](auto& i) {
               if (auto entry = Allocator::Find(i))
                  DecvqAllCast(entry)->AddRef(1);
            });

            if constexpr (REF_INDIVIDUAL and CT::Referenced<Decay<T>>)
               DenseCast(ptr).Reference(1);
         }
      }
   #endif

      /// Deep-reference an element                                           
      ///   @tparam FIND_MISSING if an entry is missing, we attempt at finding
      ///      it in the memory manager, if MANAGED_MEMORY feature is enabled.
      ///      Ignored if container is marked Emergent.                       
      ///   @attention individuals will be referenced if REF_INDIVIDUAL is    
      ///      enabled, regardless if an entry was found.                     
      ///   @attention works on one dimension at a time!                      
      template<bool FIND_MISSING = false, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void KeepElementDeep(this C& self) assumptious {
         if constexpr (requires { C::Emergent; }) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               ThisCom::template KeepElementDeepCustomPointersEmergent<SID>();
            #else
               ThisCom::template KeepElementDeepStandardPointersEmergent<SID>();
            #endif
         }
         else {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               ThisCom::template KeepElementDeepCustomPointers<FIND_MISSING, SID>();
            #else
               ThisCom::template KeepElementDeepStandardPointers<FIND_MISSING, SID>();
            #endif
         }
      }

      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      /// Relies on predeclared array of GetEntriesInner (i.e. not emergent). 
      ///   @tparam FORCE_DESTROY Destroy dense elements if true. Note:       
      ///      sparse elements are always destroyed if fully dereferenced.    
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there's exactly 1 use of the allocation!       
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state or entry            
      ///   @attention works on one dimension at a time!                      
      template<bool FORCE_DESTROY = true, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementDeepStandardPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.template GetAllocation<SID>(),
               "Can't destroy anything in a container without ownership");
            LglsAssumeDev(not FORCE_DESTROY or self.template GetUses<SID>() == 1,
               "Can't destroy data used from multiple locations");
            if (self.template IsEmpty<SID>())
               return;
         }

         using H = Decay<decltype(LglsFake(DecideHandle<C>).template PickDimension<SID>())>;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Destroying a type-erased element                         
            const auto T = self.template GetType<SID>();
            if (T.IsSparse()) {
               auto entries = self.template GetEntriesInner<SID>();
               if (not entries)
                  return;

               // If T is Text**, subT is Text*                         
               const auto subT = T.GetDeptr();
               // If T is Text**, ptr becomes Text**                    
               const auto ptr = *static_cast<void**>(self.template GetRaw<SID>());
               LglsAssumeDevAndOptimize(ptr, "Null pointer");

               if (*entries and 1 == (*entries)->GetUses()) {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     if (auto subEntry = entries + 1) {
                        H temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<FORCE_DESTROY>();
                     }
                  }
                  else if (auto destructor = subT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (REF_INDIVIDUAL) {
                        if (const auto referencer = subT.GetReferencer()) {
                           if (referencer(ptr, -1) == 0)
                              destructor(ptr);
                        }
                        else destructor(ptr);
                     }
                     else destructor(ptr);
                  }
               }
               else {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Dereference all indirection layers.             
                     if (auto subEntry = entries + 1) {
                        H temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<FORCE_DESTROY>();
                     }
                  }
                  else if constexpr (REF_INDIVIDUAL) {
                     if (const auto referencer = subT.GetReferencer()) {
                        // This element occurs in more than one place.  
                        // We're not allowed to deallocate the memory   
                        // behind it, but we must call destructors if T 
                        // is referencable and its individual references
                        // have reached 0. This can happen when hive    
                        // elements are dereferenced, for example.      
                        if (referencer(ptr, -1) == 0)
                           subT.GetDestructor()(ptr);
                     }
                  }
               }

               // Deallocate or dereference                             
               if (*entries) {
                  auto& mutable_entries = DecvqAllCast(*entries);
                  if (1 == (*entries)->GetUses())
                     Allocator::Deallocate(mutable_entries);
                  else
                     mutable_entries->AddRef(-1);
                  //mutable_entries = nullptr; //not allowed! we may be modifying memory owned by another container!!
               }
            }
            else if constexpr (FORCE_DESTROY) {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.template GetRaw<SID>();
                  if constexpr (REF_INDIVIDUAL) {
                     IF_SAFE(if (const auto referencer = T.GetReferencer())
                        referencer(ptr, -1));
                  }
                  destructor(ptr);
               }
            }
         }
         else {
            //                                                          
            // Destroying a statically-typed element                    
            using T = TypeOf<C, SID>;
            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               auto entries = self.template GetEntriesInner<SID>();
               if (not entries)
                  return;

               auto& ptr = *self.template GetRawAs<T, SID>();
               if (not ptr)
                  return;

               if (*entries and 1 == (*entries)->GetUses()) {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     typename H::Denser temp{ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<FORCE_DESTROY>();
                  }
                  else if constexpr (CT::Destroyable<DT>) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
                        if (ptr->Reference(-1) == 0)
                           ptr->~DT();
                     }
                     else ptr->~DT();
                  }
               }
               else {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     typename H::Denser temp {ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<FORCE_DESTROY>();
                  }
                  else if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     if (ptr->Reference(-1) == 0)
                        ptr->~DT();
                  }
               }

               // Deallocate or dereference                             
               if (*entries) {
                  auto& mutable_entries = DecvqAllCast(*entries);
                  if (1 == (*entries)->GetUses())
                     Allocator::Deallocate(mutable_entries);
                  else
                     mutable_entries->AddRef(-1);
                  //mutable_entries = nullptr; //not allowed! we may be modifying memory owned by another container!!
               }
            }
            else if constexpr (FORCE_DESTROY and CT::Destroyable<T>) {
               // Call destructor of dense element                      
               auto& element = self.Get();
               IF_SAFE(if constexpr (REF_INDIVIDUAL and CT::Referenced<T>)
                  element.Reference(-1));
               element.~T();
            }
         }
      }

      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      /// Emergent - every indirection will be sought in the memory manager   
      /// if MANAGED_MEMORY is enabled.                                       
      ///   @attention individuals will be dereferenced if REF_INDIVIDUAL is  
      ///      enabled, regardless if an entry was found.                     
      ///   @tparam FORCE_DESTROY Destroy dense elements if true. Note:       
      ///      sparse elements are always destroyed if fully dereferenced.    
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state or entry            
      ///   @attention works on one dimension at a time!                      
      template<bool FORCE_DESTROY = true, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementDeepStandardPointersEmergent(this C& self) assumptious {
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
         Allocation const* entry = nullptr;
         #endif

         using H = Decay<decltype(LglsFake(DecideHandle<C>).template PickDimension<SID>())>;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Destroying a type-erased element                         
            const auto T = self.template GetType<SID>();
            if (T.IsSparse()) {
               const auto subT = T.GetDeptr();
               const auto ptr = *static_cast<void**>(self.template GetRaw<SID>());
               LglsAssumeDevAndOptimize(ptr, "Null pointer");

               #if LANGULUS_FEATURE(MANAGED_MEMORY)
               entry = Allocator::Find(ptr);

               if (entry and 1 == entry->GetUses()) {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     H temp {ptr, nullptr, subT};
                     temp.template DestroyElementDeepStandardPointersEmergent<FORCE_DESTROY>();
                  }
                  else if (auto destructor = subT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (REF_INDIVIDUAL) {
                        if (const auto referencer = subT.GetReferencer()) {
                           if (referencer(ptr, -1) == 0)
                              destructor(ptr);
                        }
                        else destructor(ptr);
                     }
                     else destructor(ptr);
                  }
               }
               else {
               #endif
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Dereference all indirection layers.             
                     H temp {ptr, nullptr, subT};
                     temp.template DestroyElementDeepStandardPointersEmergent<FORCE_DESTROY>();
                  }
                  else if constexpr (REF_INDIVIDUAL) {
                     if (const auto referencer = subT.GetReferencer()) {
                        // This element occurs in more than one place.  
                        // We're not allowed to deallocate the memory   
                        // behind it, but we must call destructors if T 
                        // is referencable and its individual references
                        // have reached 0. This can happen when hive    
                        // elements are dereferenced, for example.      
                        if (referencer(ptr, -1) == 0)
                           subT.GetDestructor()(ptr);
                     }
                  }
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
               }

               // Deallocate or dereference                             
               if (entry) {
                  if (1 == entry->GetUses())
                     Allocator::Deallocate(DecvqAllCast(entry));
                  else
                     DecvqAllCast(entry)->AddRef(-1);
               }
               #endif
            }
            else if constexpr (FORCE_DESTROY) {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.template GetRaw<SID>();
                  if constexpr (REF_INDIVIDUAL) {
                     IF_SAFE(if (const auto referencer = T.GetReferencer())
                        referencer(ptr, -1));
                  }
                  destructor(ptr);
               }
            }
         }
         else {
            //                                                          
            // Destroying a statically-typed element                    
            using T = TypeOf<C, SID>;
            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               auto& ptr = *self.template GetRawAs<T, SID>();
               if (not ptr)
                  return;

               #if LANGULUS_FEATURE(MANAGED_MEMORY)
               entry = Allocator::Find(ptr);

               if (entry and 1 == entry->GetUses()) {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     typename H::Denser temp{ptr, nullptr};
                     temp.template DestroyElementDeepStandardPointersEmergent<FORCE_DESTROY>();
                  }
                  else if constexpr (CT::Destroyable<DT>) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
                        if (ptr->Reference(-1) == 0)
                           ptr->~DT();
                     }
                     else ptr->~DT();
                  }
               }
               else {
               #endif
               if constexpr (CT::Sparse<DT>) {
                  // Pointer to pointer.                             
                  // Destroy all nested indirection layers.          
                  typename H::Denser temp {ptr, nullptr};
                  temp.template DestroyElementDeepStandardPointersEmergent<FORCE_DESTROY>();
               }
               else if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
                  // This element occurs in more than one place.     
                  // We're not allowed to deallocate the memory      
                  // behind it, but we must call destructors if T is 
                  // referencable and its individual references have 
                  // reached 0. This can happen when hive elements   
                  // are dereferenced.                               
                  if (ptr->Reference(-1) == 0)
                     ptr->~DT();
               }
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
               }

               // Deallocate or dereference                             
               if (entry) {
                  if (1 == entry->GetUses())
                     Allocator::Deallocate(DecvqAllCast(entry));
                  else
                     DecvqAllCast(entry)->AddRef(-1);
               }
               #endif
            }
            else if constexpr (FORCE_DESTROY and CT::Destroyable<T>) {
               // Call destructor of dense element                      
               auto& element = self.Get();
               IF_SAFE(if constexpr (REF_INDIVIDUAL and CT::Referenced<T>)
                  element.Reference(-1));
               element.~T();
            }
         }
      }
      
   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      /// Relies on predeclared array of GetEntriesInner (i.e. not emergent). 
      ///   @tparam FORCE_DESTROY Destroy dense elements if true. Note:       
      ///      sparse elements are always destroyed if fully dereferenced.    
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there's exactly 1 use of the allocation!       
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<bool FORCE_DESTROY = true, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementDeepCustomPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.template GetAllocation<SID>(),
               "Can't destroy anything in a container without ownership");
            LglsAssumeDev(not FORCE_DESTROY or self.template GetUses<SID>() == 1,
               "Can't destroy data used from multiple locations");
            if (self.template IsEmpty<SID>())
               return;
         }

         //                                                             
         // Destroying a type-erased element                            
         auto T = self.template GetType<SID>();
         if (T.IsSparse()) {
            auto entries = self.template GetEntriesInner<SID>(); //TODO this needs to be GetEntries and it should account for entries for different types being in the same heap allocation
            if (not entries)
               return;
            
            void const* src = self.template GetRaw<SID>();
            while (src and T.IsSparse()) {
               auto nextT = T.GetDeptr();
               const bool nextDense = nextT.IsDense();
               if (not nextDense) {
                  // Pointer T -> Pointer nextT                         
                  T.GetDereffer()(const_cast<void*>(src), &src);
               }
               else if (*entries and 1 == (*entries)->GetUses()) {
                  // Pointer T -> Dense nextT, with license to destroy  
                  if (auto destructor = nextT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     src = UnpackPointer(T, nextT, src);
                     if constexpr (REF_INDIVIDUAL) {
                        if (const auto referencer = nextT.GetReferencer()) {
                           if (referencer(const_cast<void*>(src), -1) == 0)
                              destructor(const_cast<void*>(src));
                        }
                        else destructor(const_cast<void*>(src));
                     }
                     else destructor(const_cast<void*>(src));
                  }
               }
               else if constexpr (REF_INDIVIDUAL) {
                  // Pointer T -> Dense nextT, destroy only if deref    
                  if (const auto referencer = nextT.GetReferencer()) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     src = UnpackPointer(T, nextT, src);
                     if (src and referencer(const_cast<void*>(src), -1) == 0)
                        nextT.GetDestructor()(const_cast<void*>(src));
                  }
               }

               // Deallocate or dereference                             
               if (*entries) {
                  auto& mutable_entries = DecvqAllCast(*entries);
                  if (1 == (*entries)->GetUses()) {
                     Allocator::Deallocate(mutable_entries);
                     //mutable_entries = nullptr; //TODO allowed, but probably not necessary?
                  }
                  else {
                     mutable_entries->AddRef(-1);
                     //mutable_entries = nullptr; //not allowed!
                  }
               }

               // Move to next indirection                              
               T = nextT;
               ++entries;
            }
         }
         else if constexpr (FORCE_DESTROY) {
            if (const auto destructor = T.GetDestructor()) {
               // Call destructor of dense element                      
               void* const ptr = self.template GetRawVoid<SID>();
               if constexpr (REF_INDIVIDUAL) {
                  IF_SAFE(if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1));
               }
               destructor(ptr);
            }
         }
      }

      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      /// Emergent - every indirection will be sought in the memory manager   
      /// if MANAGED_MEMORY is enabled.                                       
      ///   @attention individuals will be dereferenced if REF_INDIVIDUAL is  
      ///      enabled, regardless if an entry was found.                     
      ///   @tparam FORCE_DESTROY Destroy dense elements if true. Note:       
      ///      sparse elements are always destroyed if fully dereferenced.    
      ///   @attention doesn't change any container state                     
      ///   @attention works on one dimension at a time!                      
      template<bool FORCE_DESTROY = true, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementDeepCustomPointersEmergent(this C& self) assumptious {
         //                                                             
         // Destroying a type-erased element                            
         auto T = self.template GetType<SID>();
         if (T.IsSparse()) {
            void const* src = self.template GetRaw<SID>();

            while (src and T.IsSparse()) {
               auto entry = Allocator::Find(src);
               auto nextT = T.GetDeptr();
               const bool nextDense = nextT.IsDense();
               if (not nextDense) {
                  // Pointer T -> Pointer nextT                         
                  T.GetDereffer()(const_cast<void*>(src), &src);
               }
               else if (entry and 1 == entry->GetUses()) {
                  // Pointer T -> Dense nextT, with license to destroy  
                  if (auto destructor = nextT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     src = UnpackPointer(T, nextT, src);
                     if constexpr (REF_INDIVIDUAL) {
                        if (const auto referencer = nextT.GetReferencer()) {
                           if (referencer(const_cast<void*>(src), -1) == 0)
                              destructor(const_cast<void*>(src));
                        }
                        else destructor(const_cast<void*>(src));
                     }
                     else destructor(const_cast<void*>(src));
                  }
               }
               else if constexpr (REF_INDIVIDUAL) {
                  // Pointer T -> Dense nextT, destroy only if deref    
                  if (const auto referencer = nextT.GetReferencer()) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     src = UnpackPointer(T, nextT, src);
                     if (src and referencer(const_cast<void*>(src), -1) == 0)
                        nextT.GetDestructor()(const_cast<void*>(src));
                  }
               }

               // Deallocate or dereference                             
               if (entry) {
                  if (1 == entry->GetUses())
                     Allocator::Deallocate(DecvqAllCast(entry));
                  else
                     DecvqAllCast(entry)->AddRef(-1);
               }

               // Move to next indirection                              
               T = nextT;
            }
         }
         else if constexpr (FORCE_DESTROY) {
            if (const auto destructor = T.GetDestructor()) {
               // Call destructor of dense element                      
               void* const ptr = self.template GetRawVoid<SID>();
               if constexpr (REF_INDIVIDUAL) {
                  IF_SAFE(if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1));
               }
               destructor(ptr);
            }
         }
      }
   #endif

      template<bool FORCE_DESTROY = true, Cid SID = ID, CT::Container C> requires Relevant<SID>
      void DestroyElementDeep(this C& self) assumptious {
         if constexpr (requires { C::Emergent; }) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               ThisCom::template DestroyElementDeepCustomPointersEmergent<FORCE_DESTROY, SID>();
            #else
               ThisCom::template DestroyElementDeepStandardPointersEmergent<FORCE_DESTROY, SID>();
            #endif
         }
         else {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               ThisCom::template DestroyElementDeepCustomPointers<FORCE_DESTROY, SID>();
            #else
               ThisCom::template DestroyElementDeepStandardPointers<FORCE_DESTROY, SID>();
            #endif
         }
      }

      /// Emplace on top of the first element using an intent                 
      ///   @attention Works in one dimension at a time!                      
      ///   @attention This overwrites previous entries without dereferencing 
      ///   @attention Emplacing using a handle is faster due to carrying     
      ///      allocation data with itself when sparse, rather than searching 
      ///      for it on demand.                                              
      ///   @attention Items (not entries) are referenced even if disowned,   
      ///      when REF_INDIVIDUAL is enabled and items are CT::Referenced.   
      ///   @param intent entries will be copied/sought if handle/sparse,     
      ///      unless I is disowned                                           
      template<Cid SID = ID, CT::Container C, CT::Intent I>
      requires(Relevant<SID> and (CT::TypeErased<C> or CT::Sparse<TypeOf<C, SID>>))
      void EmplaceEntries(this C& self, I&& intent) {
         if constexpr (CT::TypeErased<C>) {
            // If container is type-erased, we need to make a runtime   
            // sparsity check for an early exit.                        
            if (not self.template IsSparse<SID>())
               return;
         }

         static_assert(not CT::Cloned<I>,
            "EmplaceEntries shouldn't be called when cloning, "
            "because it will overwrite/reference new allocations"
         );

         decltype(auto) rhs = LglsFwd(intent.what);
         const auto indirections = self.template GetIndirections<SID>();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         const auto entries = self.template GetEntriesInner<SID>();

         if constexpr (CT::Handle<I>) {
            // Copy all entries and reference them, unless we're moving 
            // a handle                                                 
            using H = TypeOf<I>;
            LglsAssumeDev(self.template IsSame<SID>(rhs.template GetType<SID>()),
               "Type mismatch", ": ", self.template GetType<SID>(),
               " is not same as ", rhs.template GetType<SID>()
            );

            if constexpr (not CT::Disowned<I>
            and requires { rhs.template GetEntriesInner<SID>(); }) {
               // We can copy entries from RHS handle                   
               auto entries_src = rhs.template GetEntriesInner<SID>();
               if (entries_src) {
                  memcpy(DecvqAllCast(entries), entries_src, entries_size);

                  if constexpr (CT::StronglyOwned<H> and I::IsMoved()) {
                     // We are moving/abandoning, and we have to make   
                     // sure that source entries are zeroes, because    
                     // otherwise they will be dereferenced when H goes 
                     // out of scope.                                   
                     LglsAssumeDev(rhs.template GetUses<SID>() == 1,
                        "Can't move out from used memory");
                     memset(DecvqAllCast(entries_src), 0, entries_size);
                  }
               }
            }

            if constexpr (not I::IsMoved()) {
               // We are not moving, so we have to reference all        
               // elements.                                             
               ThisCom::template KeepElementDeep<false, SID>();
            }
            else if constexpr (CT::StronglyOwned<H> and REF_INDIVIDUAL) {
               // We are moving/abandoning, but since individual items  
               // are referenced (even if they have no corresponding    
               // entry), we need to zero the source pointer, so that   
               // we avoid them getting dereferenced later.             
               if (rhs.IsSparse()) {
                  LglsAssumeDev(rhs.template GetUses<SID>() == 1,
                     "Can't move out from used memory");
                  auto pointers_src = rhs.template GetRaw<SID>();
                  memset(DecvqAllCast(pointers_src), 0, rhs.template GetBytesize<SID>());
               }
            }
            else if constexpr (requires { Deref<H>::Emergent; }) {
               // We are moving/abandoning, but since rhs is emergent,  
               // we need to zero the source pointer, so that we avoid  
               // them getting dereferenced later.                      
               if (rhs.IsSparse()) {
                  auto pointers_src = rhs.template GetRaw<SID>();
                  memset(DecvqAllCast(pointers_src), 0, rhs.template GetBytesize<SID>());
               }
            }
         }
         else if constexpr (CT::Sparse<Deint<I>>) {
            // Reference each indirection of a raw pointer              
            #if LANGULUS_FEATURE(MANAGED_MEMORY) or LANGULUS(SAFE) > 1
            using T = Decvq<Deref<Deint<I>>>;
            LglsAssumeDev((self.template IsSame<T, SID>()),
               "Type mismatch", ": ", self.template GetType<SID>(),
               " is not same as ", NameOf<T>()
            );
            #endif

            // We're forced to reference even on abandon/move           
            // because we can't abandon/move a raw pointer. Missing     
            // entries will be sought and referenced as well, unless    
            // inserted pointer is disowned.                            
            constexpr bool sought = not CT::Disowned<I>;
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               if constexpr (CT::CustomPointer<T>)
                  ThisCom::template KeepElementDeepCustomPointers<sought, SID>();
               else
                  ThisCom::template KeepElementDeepStandardPointers<sought, SID>();
            #else
               ThisCom::template KeepElementDeepStandardPointers<sought, SID>();
            #endif
         }
      }

      /// Reset all entries for the first element                             
      ///   @attention this overwrites previous entries without dereferencing 
      template<Cid SID = ID, CT::Container C>
      requires(Relevant<SID> and (CT::TypeErased<C> or CT::Sparse<TypeOf<C, SID>>))
      void ResetEntries(this C& self) {
         if constexpr (CT::TypeErased<C>) {
            // If container is type-erased, we need to make a runtime   
            // sparsity check for an early exit.                        
            if (not self.template IsSparse<SID>())
               return;
         }

         static_assert(CT::ContainsOne<C>,
            "Resetting entries for first element in a container with many. "
            "GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.template GetUses<SID>() == 1,
               "ResetEntries shouldn't be called for shared memory");
         }

         const auto indirections = self.template GetIndirections<SID>();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         auto entries = self.template GetEntriesInner<SID>();
         memset(DecvqAllCast(entries), 0, entries_size);
      }

      /// Called on container destruction                                     
      ///   @attention this never modifies any state                          
      ///   @attention operates on all relevant dimensions at once!           
      template<class C>
      void Destroy(this C& self) noexcept {
         if (self.IsEmpty())
            return;

         self.Apply([&](auto&& item) {
            Id::ForEach([&]<Cid D> {//TODO take dimension outside loop for less context switching overhead
               if constexpr (CT::TypeErased<C>) {
                  if (self.template IsSparse<D>())
                     item.template DestroyElementDeep<false, D>();
               }
               else {
                  if constexpr (CT::Sparse<TypeOf<C, D>>)
                     item.template DestroyElementDeep<false, D>();
               }
            });
         });
      }
   };

   #undef ThisCom
}
