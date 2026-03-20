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
   ///                                                                        
   /// Manages deep ownership by searching for an allocation every time.      
   /// Also used as base for other deep ownership components.                 
   ///   @tparam ID which heap/stack are we keeping track of?                 
   ///   @tparam REF_INDIVIDUAL toggles whether contained items that were     
   ///      reflected as CT::Referenced get referenced. Elements will get     
   ///      referenced even if no entry for the element exist, but you can    
   ///      avoid referencing altogether if you use the Disown intent.        
   ///      To be more specific - when GetReference() is nullptr and the      
   ///      entire container is considered disowned.                          
   template<Cid ID, bool REF_INDIVIDUAL>
   struct OwnershipDeepEmergent {
      using CTTI_Component = Yes<>;
      
      static constexpr bool DeeplyOwned = true;
      static constexpr bool ReferenceElements = REF_INDIVIDUAL;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<Cid, CT::Sparse>             friend struct HeapReference;
      template<Cid, uint, uint, CT::Sparse> friend struct HeapMovable;
      template<Cid>                         friend struct Removal;
      template<Cid>                         friend struct Emplacement;
      template<Cid, bool>                   friend struct OwnershipEmergent;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      /// Nests through all indirection layers and references elements and    
      /// their entries.                                                      
      ///   @tparam FIND_MISSING if an entry is missing, we attempt at finding
      ///      it in the memory manager, if MANAGED_MEMORY feature is enabled 
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state                     
      template<bool FIND_MISSING = false, CT::Container C>
      void KeepElementDeepStandardPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Referencing only first element in a container with many. GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't keep anything in a container without ownership");
         }

         if (self.IsEmpty())
            return;

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Referencing a type-erased element                        
            const auto T = self.GetType();

            if (T.IsSparse()) {
               auto entries = self.GetEntriesInner();
               if (not entries)
                  return;

               const auto subT = T.GetDeptr();
               const auto ptr = *static_cast<void**>(const_cast<void*>(self.GetRaw()));
               LglsAssumeDevAndOptimize(ptr, "Null pointer");

               if (subT.IsSparse()) {
                  // Pointer to pointer                                 
                  DecideHandle<C> temp {ptr, entries + 1, subT};
                  temp.template KeepElementDeepStandardPointers<FIND_MISSING>();
               }
               else if constexpr (REF_INDIVIDUAL) {
                  if (const auto referencer = subT.GetReferencer()) {
                     // Pointer to dense                                
                     referencer(ptr, 1);
                  }
               }

               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  if constexpr (FIND_MISSING) {
                     if (not *entries)
                        DecvqAllCast(*entries) = DecvqAllCast(Allocator::Find(ptr));
                  }
               #endif

               if (*entries)
                  DecvqAllCast(*entries)->AddRef(1);
            }
         }
         else {
            //                                                          
            // Referencing a statically-typed element                   
            using T = TypeOf<C>;         

            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               auto entries = self.GetEntriesInner();
               if (not entries)
                  return;

               auto& ptr = *self.template GetRawAs<T>();
               LglsAssumeDevAndOptimize(ptr, "Null pointer");

               if constexpr (CT::Sparse<DT>) {
                  // Pointer to pointer                                 
                  typename DecideHandle<C>::Denser temp {ptr, entries + 1};
                  temp.template KeepElementDeepStandardPointers<FIND_MISSING>();
               }
               else if constexpr (REF_INDIVIDUAL and CT::Referenced<DT>) {
                  // Pointer to dense                                   
                  ptr->Reference(1);
               }
            
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  if constexpr (FIND_MISSING) {
                     if (not *entries)
                        DecvqAllCast(*entries) = DecvqAllCast(Allocator::Find(ptr));
                  }
               #endif

               if (*entries)
                  DecvqAllCast(*entries)->AddRef(1);
            }
         }
      }


   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Nests through all indirection layers and references elements and    
      /// their entries. Supports any number or custom pointer indirections.  
      ///   @tparam FIND_MISSING if an entry is missing, we attempt at finding
      ///      it in the memory manager, if MANAGED_MEMORY feature is enabled 
      ///   @attention assumes container is not disowned!                     
      ///   @attention doesn't change any container state                     
      template<bool FIND_MISSING = false, CT::Container C>
      void KeepElementDeepCustomPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Referencing only first element in a container with many. GetHandle() first?");
         LglsAssumeDev(not self.IsEmpty(),
            "No point in calling this on an empty container");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't keep anything in a container without ownership");
         }

         // Check if disowned/outside authority                         
         auto entries = self.GetEntriesInner();
         if (not entries)
            return;

         if constexpr (CT::TypeErased<C>) {
            // Check if containing indirections                         
            DMeta T = self.GetType();
            LglsAssumeDev(T.IsSparse(), "Sparseness mismatch");

            void* src = DecvqAllCast(self.GetHeapInner());
            while (src and T.IsSparse()) {
               auto nextT = T.GetDeptr();
               if constexpr (FIND_MISSING) {
                  if (not *entries) {
                     const auto srcSpec = T.GetPointerSpecification();
                     if (srcSpec.IsPacked()) {
                        uintptr_t derefSrc = 0;
                        memcpy(&derefSrc, src, srcSpec.GetTotalBytes());
                        DecvqAllCast(*entries) =
                           DecvqAllCast(Allocator::FindPackedPointer(
                              srcSpec, nextT, derefSrc
                           ));
                     }
                     else {
                        DecvqAllCast(*entries) =
                           DecvqAllCast(Allocator::Find(*static_cast<void**>(src)));
                     }
                  }
               }

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
            using T = TypeOf<C>;
            static_assert(CT::Sparse<T>, "Sparseness mismatch");

            auto ptr = self.Get();
            ForEachIndirection(ptr, [&entries](auto& i) {
               if constexpr (FIND_MISSING) {
                  if (not *entries)
                     DecvqAllCast(*entries) = DecvqAllCast(Allocator::Find(i));
               }

               // Reference valid entries if not zero                   
               if (*entries)
                  DecvqAllCast(*entries)->AddRef(1);

               ++entries;
            });

            if constexpr (REF_INDIVIDUAL and CT::Referenced<Decay<T>>)
               DenseCast(ptr).Reference(1);
         }
      }
   #endif

      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there's exactly 1 use of the allocation!       
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state or entry            
      template<bool DESTROY = true, CT::Container C>
      void DestroyElementDeepStandardPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");
         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't destroy anything in a container without ownership");
            LglsAssumeDev(not DESTROY or self.GetUses() == 1,
               "Can't destroy data used from multiple locations");
            if (self.IsEmpty())
               return;
         }

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Destroying a type-erased element                         
            const auto T = self.GetType();
            
            if (T.IsSparse()) {
               auto entries = self.GetEntriesInner();
               if (not entries)
                  return;

               // If T is Text**, subT is Text*                         
               const auto subT = T.GetDeptr();
               // If T is Text**, ptr becomes Text**                    
               const auto ptr = *static_cast<void**>(self.GetRaw());
               LglsAssumeDevAndOptimize(ptr, "Null pointer");

               if (*entries and 1 == (*entries)->GetUses()) {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     if (auto subEntry = entries + 1) {
                        DecideHandle<C> temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<DESTROY>();
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
                        DecideHandle<C> temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<DESTROY>();
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
            else if constexpr (DESTROY) {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.GetRaw();
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
            using T = TypeOf<C>;
            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               auto entries = self.GetEntries();
               if (not entries /*or not *entries*/)
                  return;

               auto& ptr = *self.template GetRawAs<T>();
               LglsAssumeDev(ptr, "Null pointer");

               if (*entries and 1 == (*entries)->GetUses()) {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     using DenserH = typename DecideHandle<C>::Denser;
                     DenserH temp{ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<DESTROY>();
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
                     using DenserH = typename DecideHandle<C>::Denser;
                     DenserH temp {ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<DESTROY>();
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
            else if constexpr (DESTROY and CT::Destroyable<T>) {
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
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there's exactly 1 use of the allocation!       
      ///   @attention doesn't change any container state                     
      ///   @tparam DESTROY will never destroy a dense element if true        
      //TODO could use some statically-typed optimizations
      template<bool DESTROY = true, CT::Container C>
      void DestroyElementDeepCustomPointers(this C& self) assumptious {
         static_assert(CT::ContainsOne<C>,
            "Destroying only first element in a container with many. GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't destroy anything in a container without ownership");
            LglsAssumeDev(not DESTROY or self.GetUses() == 1,
               "Can't destroy data used from multiple locations");
            if (self.IsEmpty())
               return;
         }

         //                                                             
         // Destroying a type-erased element                            
         DMeta T = self.GetType();
         if (T.IsSparse()) {
            auto entries = self.GetEntriesInner();
            if (not entries)
               return;
            
            void const* src = self.GetRaw();
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
                  if (1 == (*entries)->GetUses())
                     Allocator::Deallocate(mutable_entries);
                  else
                     mutable_entries->AddRef(-1);
                  //mutable_entries = nullptr; //not allowed! we may be modifying memory owned by another container!!
               }

               // Move to next indirection                              
               T = nextT;
               ++entries;
            }
         }
         else if constexpr (DESTROY) {
            if (const auto destructor = T.GetDestructor()) {
               // Call destructor of dense element                      
               void* const ptr = self.GetHeapInnerAsVoid();
               if constexpr (REF_INDIVIDUAL) {
                  IF_SAFE(if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1));
               }
               destructor(ptr);
            }
         }
      }
   #endif

      /// Emplace on top of the first element using an intent                 
      ///   @attention this overwrites previous entries without dereferencing 
      ///   @attention emplacing using a handle is faster due to carrying     
      ///      allocation data with itself when sparse, rather than searching 
      ///      for it on demand.                                              
      ///   @attention items (not entries) are referenced even if disowned,   
      ///      when REF_INDIVIDUAL is enabled and items are CT::Referenced.   
      ///   @param intent entries will be copied/sought if handle/sparse,     
      ///      unless I is disowned                                           
      template<CT::Container C, CT::Intent I> requires (CT::TypeErased<C> or CT::Sparse<TypeOf<C>>)
      void EmplaceEntries(this C& self, I&& intent) {
         if constexpr (CT::TypeErased<C>) {
            // If container is type-erased, we need to make a runtime   
            // sparsity check for an early exit.                        
            if (not self.IsSparse())
               return;
         }

         static_assert(CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. "
            "GetHandle() first?");
         static_assert(not CT::Cloned<I>,
            "EmplaceEntries shouldn't be called when cloning, "
            "because it will overwrite/reference new allocations");

         decltype(auto) rhs = LglsFwd(intent.what);
         const auto indirections = self.GetIndirections();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         auto entries = self.GetEntriesInner();

         if constexpr (CT::Handle<I>) {
            // Copy all entries and reference them, unless we're moving 
            // a handle                                                 
            using H = TypeOf<I>;
            LglsAssumeDev(self.IsSame(rhs.GetType()),
               "Type mismatch: ", self.GetType(), " is not same as ", rhs.GetType()
            );

            if constexpr (not CT::Disowned<I>
            and requires { rhs.GetEntriesInner(); }) {
               // We can copy entries from RHS handle                   
               auto entries_src = rhs.GetEntriesInner();
               if (entries_src) {
                  memcpy(DecvqAllCast(entries), entries_src, entries_size);

                  if constexpr (CT::AutoOwned<H> and I::IsMoved()) {
                     // We are moving/abandoning, and we have to make   
                     // sure that source entries are zeroes, because    
                     // otherwise they will be dereferenced when H goes 
                     // out of scope.                                   
                     LglsAssumeDev(rhs.GetUses() == 1,
                        "Can't move out from used memory");
                     memset(DecvqAllCast(entries_src), 0, entries_size);
                  }
               }
            }

            if (not I::IsMoved()) {
               // We are not moving, so we have to reference all        
               // elements.                                             
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  self.KeepElementDeepCustomPointers();
               #else
                  self.KeepElementDeepStandardPointers();
               #endif
            }
            else if constexpr (CT::AutoOwned<H> and REF_INDIVIDUAL) {
               // We are moving/abandoning, but since individual items  
               // are referenced (even if they have no corresponding    
               // entry), we need to zero the source pointer, so that   
               // we avoid them getting dereferenced later.             
               LglsAssumeDev(rhs.GetUses() == 1,
                  "Can't move out from used memory");
               auto pointers_src = rhs.GetRaw();
               memset(DecvqAllCast(pointers_src), 0, rhs.GetBytesize());
            }
         }
         else if constexpr (CT::Sparse<Deint<I>>) {
            // Reference each indirection of a raw pointer              
            using T = Decvq<Deref<Deint<I>>>;
            LglsAssumeDev(self.template IsSame<T>(),
               "Type mismatch: ", self.GetType(), " is not same as ", NameOf<T>()
            );

            // We're forced to reference even on abandon/move           
            // because we can't abandon/move a raw pointer. Missing     
            // entries will be sought and referenced as well, unless    
            // inserted pointer is disowned.                            
            constexpr bool sought = not CT::Disowned<I>;
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               if constexpr (CT::CustomPointer<T>)
                  self.template KeepElementDeepCustomPointers<sought>();
               else
                  self.template KeepElementDeepStandardPointers<sought>();
            #else
               self.template KeepElementDeepStandardPointers<sought>();
            #endif
         }
      }

      /// Reset all entries for the first element                             
      ///   @attention this overwrites previous entries without dereferencing 
      template<CT::Container C> requires (CT::TypeErased<C> or CT::Sparse<TypeOf<C>>)
      void ResetEntries(this C&& self) {
         if constexpr (CT::TypeErased<C>) {
            // If container is type-erased, we need to make a runtime   
            // sparsity check for an early exit.                        
            if (not self.IsSparse())
               return;
         }

         static_assert(CT::ContainsOne<C>,
            "Resetting entries for first element in a container with many. "
            "GetHandle() first?");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetUses() == 1,
               "ResetEntries shouldn't be called for shared memory");
         }

         const auto indirections = self.GetIndirections();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         auto entries = self.GetEntriesInner();
         memset(DecvqAllCast(entries), 0, entries_size);
      }
   };
}
