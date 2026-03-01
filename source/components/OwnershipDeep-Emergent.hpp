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
   /// Manages deep ownership by searching for an allocation every time       
   ///   @tparam ID which heap/stack are we keeping track of?                 
   template<Cid ID>
   struct OwnershipDeepEmergent {
      using CTTI_Component = Yes<>;
      
      static constexpr bool DeeplyOwned = true;
      static constexpr int  ComponentPrecedence = 2000;

   protected:
      template<Cid, CT::Sparse>                     friend struct HeapReference;
      template<Cid, unsigned, unsigned, CT::Sparse> friend struct HeapMovable;
      template<Cid>                                 friend struct Removal;
      template<Cid>                                 friend struct Emplacement;
      template<Cid, bool, bool>                     friend struct OwnershipEmergent;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      /// Nests through all indirection layers and references elements and    
      /// their entries.                                                      
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state                     
      template<CT::Container C>
      void KeepElementDeepStandardPointers(this C& self) assumptious {
         static_assert(CT::DeeplyOwned<C>,
            "Shouldn't be called in shallow owned containers");
         static_assert(CT::ContainsOne<C>,
            "Referencing only first element in a container with many");
         LglsAssumeDev(self.GetAllocation(),
            "Can't keep anything in a container without ownership");
         if (self.IsEmpty())
            return;

         using H = typename C::HandleMutType;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Referencing a type-erased element                        
            const auto T = self.GetType();
            if (T.IsSparse()) {
               AllocationPtr* entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               const auto subT = T.GetDeptr();
               const auto ptr = *static_cast<void**>(const_cast<void*>(self.GetRaw()));
               LglsAssumeDev(ptr, "Null pointer");

               if (subT.IsSparse()) {
                  // Pointer to pointer                                 
                  if (auto subEntry = entries + 1) {
                     H temp {ptr, subEntry, subT};
                     temp.KeepElementDeepStandardPointers();
                  }
               }
               else if (const auto referencer = subT.GetReferencer()) {
                  // Pointer to dense                                   
                  referencer(ptr, 1);
               }

               DecvqAllCast(*entries)->AddRef(1);
            }
         }
         else {
            //                                                          
            // Referencing a statically-typed element                   
            using T = TypeOf<C>;            
            if constexpr (CT::Sparse<T>) {
               using DT = Deptr<T>;
               AllocationPtr* entries = self.GetEntries();
               if (not entries or not *entries)
                  return;

               auto& ptr = *self.template GetRawAs<T>();
               LglsAssumeDev(ptr, "Null pointer");

               if constexpr (CT::Sparse<DT>) {
                  // Pointer to pointer                                 
                  using DenserH = typename H::Denser;
                  DenserH temp {ptr, entries + 1};
                  temp.KeepElementDeepStandardPointers();
               }
               else if constexpr (CT::Referenced<DT>) {
                  // Pointer to dense                                   
                  ptr->Reference(1);
               }

               DecvqAllCast(*entries)->AddRef(1);
            }
         }
      }      

      /// Nests through all indirection layers and references elements and    
      /// their entries. Supports any number or custom pointer indirections.  
      ///   @attention assumes container is not disowned!                     
      ///   @attention doesn't change any container state                     
      //TODO could use some statically-typed optimizations
      template<CT::Container C>
      void KeepElementDeepCustomPointers(this C& self) assumptious {
         static_assert(CT::DeeplyOwned<C>,
            "Shouldn't be called in shallow owned containers");
         static_assert(CT::ContainsOne<C>,
            "Referencing only first element in a container with many");
         LglsAssumeDev(not self.IsEmpty(),
            "No point in calling this on an empty container");

         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't keep anything in a container without ownership");
         }

         // Check if containing indirections                            
         DMeta T = self.GetType();
         auto indirections = T.GetIndirections();
         if (not indirections)
            return;

         // Check if disowned/outside authority                         
         auto entries = self.GetEntriesInner();
         if (not entries)
            return;

         void const* src = self.GetRaw();
         while (*entries and src and T.IsSparse()) {            
            auto nextT = T.GetDeptr();            
            if (nextT.IsSparse()) {
               // Pointer T -> Pointer nextT                            
               T.GetDereffer()(const_cast<void*>(src), &src);
            }
            else if (const auto referencer = nextT.GetReferencer()) {
               // Pointer T -> Dense nextT                              
               referencer(const_cast<void*>(UnpackPointer(T, nextT, src)), 1);
            }

            DecvqAllCast(*entries)->AddRef(1);

            // Move to next indirection                                 
            T = nextT;
            entries += 1;
         }
      }      

      /// Nests through all indirection layers and destroys elements and      
      /// their entries if they are fully dereferenced.                       
      ///   @attention assumes container is not disowned!                     
      ///   @attention assumes there's exactly 1 use of the allocation!       
      ///   @attention assumes there are no custom pointers involved!         
      ///   @attention doesn't change any container state                     
      template<bool DESTROY = true, CT::Container C>
      void DestroyElementDeepStandardPointers(this C& self) assumptious {
         static_assert(CT::DeeplyOwned<C>,
            "Shouldn't be called in shallow owned containers");

         //static_assert(CT::ContainsOne<C>,
         //  "Destroying only first element in a container with many");
         if constexpr (not CT::Handle<C>) {
            LglsAssumeDev(self.GetAllocation(),
               "Can't destroy anything in a container without ownership");
            LglsAssumeDev(not DESTROY or self.GetUses() == 1,
               "Can't destroy data used from multiple locations");
            if (self.IsEmpty())
               return;
         }

         using H = typename C::HandleMutType;
         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // Destroying a type-erased element                         
            const auto T = self.GetType();
            
            if (T.IsSparse()) {
               auto entries = self.GetEntriesInner();
               if (not entries or not *entries)
                  return;

               // If T is Text**, subT is Text*                         
               const auto subT = T.GetDeptr();
               // If T is Text**, ptr becomes Text**                    
               const auto ptr = *static_cast<void**>(self.GetRaw());
               LglsAssumeDev(ptr, "Null pointer");

               if (1 == (*entries)->GetUses()) {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     if (auto subEntry = entries + 1) {
                        H temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<DESTROY>();
                     }
                  }
                  else if (auto destructor = subT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if (const auto referencer = subT.GetReferencer()) {
                        if (referencer(ptr, -1) == 0)
                           destructor(ptr);
                     }
                     else destructor(ptr);
                  }

                  Allocator::Deallocate(DecvqAllCast(*entries));
               }
               else {
                  if (subT.IsSparse()) {
                     // Pointer to pointer.                             
                     // Dereference all indirection layers.             
                     if (auto subEntry = entries + 1) {
                        H temp {ptr, subEntry, subT};
                        temp.template DestroyElementDeepStandardPointers<DESTROY>();
                     }
                  }
                  else if (const auto referencer = subT.GetReferencer()) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     if (referencer(ptr, -1) == 0)
                        subT.GetDestructor()(ptr);
                  }

                  DecvqAllCast(*entries)->AddRef(-1);
               }
            }
            else if constexpr (DESTROY) {
               if (const auto destructor = T.GetDestructor()) {
                  // Call destructor of dense element                   
                  const auto ptr = self.GetRaw();
                  IF_SAFE(if (const auto referencer = T.GetReferencer())
                     referencer(ptr, -1));
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
               if (not entries or not *entries)
                  return;

               auto& ptr = *self.template GetRawAs<T>();
               LglsAssumeDev(ptr, "Null pointer");

               if (1 == (*entries)->GetUses()) {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     using DenserH = typename H::Denser;
                     DenserH temp{ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<DESTROY>();
                  }
                  else if constexpr (CT::Destroyable<DT>) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     if constexpr (CT::Referenced<DT>) {
                        if (ptr->Reference(-1) == 0)
                           ptr->~DT();
                     }
                     else ptr->~DT();
                  }

                  Allocator::Deallocate(DecvqAllCast(*entries));
               }
               else {
                  if constexpr (CT::Sparse<DT>) {
                     // Pointer to pointer.                             
                     // Destroy all nested indirection layers.          
                     using DenserH = typename H::Denser;
                     DenserH temp {ptr, entries + 1};
                     temp.template DestroyElementDeepStandardPointers<DESTROY>();
                  }
                  else if constexpr (CT::Referenced<DT>) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     if (ptr->Reference(-1) == 0)
                        ptr->~DT();
                  }

                  DecvqAllCast(*entries)->AddRef(-1);
               }
            }
            else if constexpr (DESTROY and CT::Destroyable<T>) {
               // Call destructor of dense element                      
               auto& element = self.Get();
               IF_SAFE(if constexpr (CT::Referenced<T>)
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
         static_assert(CT::DeeplyOwned<C>,
            "Shouldn't be called in shallow owned containers");

         //static_assert(CT::ContainsOne<C>,
         //   "Destroying only first element in a container with many");

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
            while (*entries and src and T.IsSparse()) {
               auto nextT = T.GetDeptr();
               const bool nextDense = nextT.IsDense();
               if (not nextDense) {
                  // Pointer T -> Pointer nextT                         
                  T.GetDereffer()(const_cast<void*>(src), &src);
               }
               else if (1 == (*entries)->GetUses()) {
                  // Pointer T -> Dense nextT                           
                  if (auto destructor = nextT.GetDestructor()) {
                     // Pointer to a complete, destroyable dense.       
                     // Call the destructor.                            
                     src = UnpackPointer(T, nextT, src);
                     if (const auto referencer = nextT.GetReferencer()) {
                        if (referencer(const_cast<void*>(src), -1) == 0)
                           destructor(const_cast<void*>(src));
                     }
                     else destructor(const_cast<void*>(src));
                  }
               }
               else {
                  // Pointer T -> Dense nextT                           
                  if (const auto referencer = nextT.GetReferencer()) {
                     // This element occurs in more than one place.     
                     // We're not allowed to deallocate the memory      
                     // behind it, but we must call destructors if T is 
                     // referencable and its individual references have 
                     // reached 0. This can happen when hive elements   
                     // are dereferenced.                               
                     src = UnpackPointer(T, nextT, src);
                     if (referencer(const_cast<void*>(src), -1) == 0)
                        nextT.GetDestructor()(const_cast<void*>(src));
                  }
               }

               // Deallocate or dereference                             
               if (1 == (*entries)->GetUses())
                  Allocator::Deallocate(DecvqAllCast(*entries));
               else
                  DecvqAllCast(*entries)->AddRef(-1);

               // Move to next indirection                              
               T = nextT;
               ++entries;
            }
         }
         else if constexpr (DESTROY) {
            if (const auto destructor = T.GetDestructor()) {
               // Call destructor of dense element                      
               void* const ptr = self.GetHeapInnerAsVoid();
               IF_SAFE(if (const auto referencer = T.GetReferencer())
                  referencer(ptr, -1));
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
      ///   @param intent entries will be copied/sought if handle/sparse      
      template<CT::Container C, CT::Intent I> requires CT::DeeplyOwned<C>
      void EmplaceEntries(this C& self, I&& intent) {
         //static_assert(CT::DeeplyOwned<C>,
         //   "Shouldn't be called in shallow owned containers");
         static_assert(not CT::Cloned<I>,
            "EmplaceEntries shouldn't be called when cloning, "
            "because it will overwrite/reference new allocations");
         LglsAssumeDev(self.IsSparse(),
            "EmplaceEntries shouldn't be called on dense containers");
         using IT = Decvq<Deref<TypeOf<I>>>;
         decltype(auto) rhs = LglsFwd(intent.what);

         if constexpr (CT::Handle<IT>)
            LglsAssumeDev(rhs.IsSparse(), "Sparseness mismatch");
         else
            LglsAssumeDev(CT::Sparse<decltype(rhs)>, "Sparseness mismatch");

         const auto indirections = self.GetIndirections();
         const auto entries_size = sizeof(AllocationPtr) * indirections;
         auto entries = self.GetEntriesInner();

         if constexpr ((CT::Handle<IT> and     I::IsKept())
         or (       not CT::Handle<IT> and not CT::Disowned<I>)) {
            // When it's a keeping intent, copy all entries and         
            // reference them                                           
            if constexpr (CT::Handle<IT>) {
               if (auto entries_src = rhs.GetEntries())
                  memcpy(DecvqAllCast(entries), entries_src, entries_size);
               else {
                  // RHS might be a disowned handle                     
                  memset(DecvqAllCast(entries), 0, entries_size);
               }
            }
            else memset(DecvqAllCast(entries), 0, entries_size);

            if constexpr (CT::Handle<IT> or LANGULUS_FEATURE(MANAGED_MEMORY)) {
               auto const entriesEnd = entries + indirections;
               auto meta = self.GetType().GetDeptr();
               void** handle = self.template GetRawAs<void*>(); //TODO this won't work with packed pointers, would it?

               while (entries < entriesEnd) {
                  // When it's a keeping intent, copy all entries and   
                  // reference them. Notice that when NOT emplacing via 
                  // a handle, we're forced to reference on abandon,    
                  // because we can't abandon a raw pointer.            
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     if constexpr (not CT::Handle<IT>)
                        const_cast<AllocationPtr&>(*entries) = Allocator::Find(*handle);
                  #endif

                  if (not *entries)
                     break;

                  DecvqAllCast(*entries)->AddRef(1);

                  LglsAssumeDev(meta,
                     "Valid entry, but invalid type");
                  LglsAssumeDevAndOptimize(*handle,
                     "Valid entry, but invalid pointer");

                  auto referencer = meta.GetReferencer();
                  if (meta.IsDense() and referencer)
                     referencer(*handle, 1);

                  handle = reinterpret_cast<void**>(*handle); //TODO this won't work with packed pointers, would it?
                  meta = meta.GetDeptr();
                  ++entries;
               }
            }
         }
         else {
            // Disowning just zeroes all entries                        
            memset(DecvqAllCast(entries), 0, entries_size);
         }
      }
   };
}
