///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Langulus/CT/Contiguous.hpp"
#include <Langulus/CT/Unfold.hpp>
#include <Langulus/CT/ReflectAs.hpp>


namespace Langulus::CT
{
   /// Check if container's elements are unfold-assignable                    
   ///   @attention type-erased elements are always assignable, and will fail 
   ///      at runtime if not reflected as such                               
   ///   @attention we allow a fallback for elements that are not assignable, 
   ///      but constructible - this is detected by the container, and we can 
   ///      just destroy and reconstruct the element in its place.            
   template<class C, class A>
   concept RangeAssignable = Container<C> and (
      Untyped<C> or UnfoldAssignable<TypeOf<C>, A>
                 or UnfoldConstructible<TypeOf<C>, A>
   );

   namespace Inner
   {
      /// Test whether a container is assignable with the given argument      
      ///   @tparam C the container                                           
      ///   @tparam A the argument to test                                    
      ///   @return true if you can assign A to the container                 
      template<Container C, class A>
      consteval bool DeepAssignable() noexcept {
         using SA = IntentOfT<A>;
         using T  = TypeOf<C>;

         if constexpr (Untyped<C>) {
            // Type-erased containers accept almost any type - they     
            // will report errors at runtime instead, if any            
            return Reflectable<Deint<A>>;
         }
         else if constexpr (Container<A>) {
            if constexpr (SA::Shallow) {
               // Generally, shallow intents are always supported, but  
               // copying will call element assigners, so we have to    
               // check if the contained type supports it               
               if constexpr (Copied<SA>)
                  return ReferAssignable<T>;
               else
                  return true;
            }
            else {
               // Cloning always calls element assigners, and we have   
               // to check whether contained elements can do it         
               return IntentAssignable<Langulus::Clone, T>;
            }
         }
         else return UnfoldAssignable<T, A>;
      };
   }

   /// Concept for recognizing argument with which a statically typed         
   /// container can be assigned                                              
   template<class C, class A>
   concept DeepAssignable = Inner::DeepAssignable<C, A>();
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements element assignment for containers.                          
   /// Assignment acts on the first element, if container is contiguous.      
   /// For discontiguous containers, like sets and maps, the assignment falls 
   /// back to insertion.                                                     
   ///   @tparam ID heap/stack we're assigning to                             
   ///   @tparam SHARED other providers that share assignment behavior        
   template<Cid ID, Cid...SHARED>
   struct Assignment {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Id = Values<ID, SHARED...>;

      static constexpr int ComponentPrecedence = 3000;
      template<Cid SID>
      static constexpr bool Relevant = Id::template Contains<SID>;

      //template<CT::Container C, class A>
      //void Fill(this C&, A&&) requires CT::RangeAssignable<C, A>;

      /// MARK: Public                                                        
      /// Assign a value to the first element, if that element is initialized.
      /// If the element isn't initialized yet it will be constructed.        
      ///   @param argument the argument to assign                            
      ///   @return reference to self                                         
      template<CT::Container C, class A>
      C& Assign(this C& self, A&& argument)
      requires (CT::RangeAssignable<C, A> /*and CT::Contiguous<C>*/) {
         if constexpr (not CT::Contiguous<C>) {
            // Assignment for maps/sets falls back to merge             
            self.Clear();
            self.Merge(LglsFwd(argument));
         }
         else if constexpr (not CT::HeapAllocated<C>) {
            // This container is on the stack, and by extension         
            // statically-typed and always initialized                  
            auto& data = self.template AccessProvider<ID>();
            data = LglsFwd(argument);
         }
         else {
            // This container is heap-allocated                         
            using T = Tif<CT::TypeErased<C>, Decvq<Deref<Deint<A>>>, TypeOf<C>>;
            self.template SetType<T>();

            if (self.IsEmpty()) {
               // Container is empty, we might have to fresh-allocate   
               if constexpr (CT::UnfoldConstructible<T, A&&>) {
                  // Just construct the first element                   
                  self.PrepareForReconstruction();

                  auto first = self.GetHandle();
                  if constexpr (CT::Copied<IntentOf(argument)>) {
                     Id::ForEach([&]<Cid D>{
                        first.template EmplaceWithIntent<D>(Refer(LglsFwd(argument)));
                     });
                  }
                  else {
                     Id::ForEach([&]<Cid D>{
                        first.template EmplaceWithIntent<D>(FWDIntent(argument));
                     });
                  }
               }
               else static_assert(false, "T can't be reconstructed");
            }
            else {
               // Container has at least one element                    
               if constexpr (not CT::Cloned<IntentOf(argument)> and CT::UnfoldAssignable<T, A&&>) {
                  // Reduce to one item and reassign if possible        
                  if (self.PrepareForReassignment()) {
                     auto first = self.GetHandle();
                     Id::ForEach([&]<Cid D>{
                        if constexpr (CT::Copied<IntentOf(argument)>)
                           first.template AssignWithIntent<D>(Refer(LglsFwd(argument)));
                        else
                           first.template AssignWithIntent<D>(FWDIntent(argument));
                     });
                  }
                  else {
                     auto first = self.GetHandle();
                     Id::ForEach([&]<Cid D>{
                        if constexpr (CT::Copied<IntentOf(argument)>)
                           first.template EmplaceWithIntent<D>(Refer(LglsFwd(argument)));
                        else
                           first.template EmplaceWithIntent<D>(FWDIntent(argument));
                     });
                  }
               }
               else if constexpr (CT::UnfoldConstructible<T, A&&>) {
                  // Assignment isn't available for T - destroy all     
                  // items and reconstruct the first one                
                  self.PrepareForReconstruction();

                  auto first = self.GetHandle();
                  Id::ForEach([&]<Cid D>{
                     if constexpr (CT::Copied<IntentOf(argument)>)
                        first.template EmplaceWithIntent<D>(Refer(LglsFwd(argument)));
                     else
                        first.template EmplaceWithIntent<D>(FWDIntent(argument));
                  });
               }
               else static_assert(false, "T can't be reassigned or reconstructed");
            }

            if_available(self.SetCountInner(1));
            if_available(self.SetHashInner(0));
         }
         
         return self;
      }

      /// Assignment for discontiguous containers falls back to insert/merge. 
      ///   @param argument the argument to insert                            
      ///   @return reference to self                                         
      /*template<CT::Container C, class A>
      C& Assign(this C& self, A&& argument)
      requires (CT::RangeAssignable<C, A> and not CT::Contiguous<C>) {
         self.Clear();
              if_available(self.Insert(LglsFwd(argument)))
         else if_available(self.Merge (LglsFwd(argument)))
         return self;
      }*/

   protected:
      /// MARK: Protected                                                     
      LglsComIndexedCommonHashed(friend);
      LglsComConversion(friend);

      /// A helper for clearing and allocating memory before construction.    
      /// Calls destructors on all elements, if any were initialized.         
      ///   @attention operates in all relevant dimensions simultaneously     
      template<CT::HeapAllocated C>
      void PrepareForReconstruction(this C& self) {
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");

         using PROVIDERS = decltype(C::FindProviders(Id{}));
         static_assert(not PROVIDERS::Empty);
         if (self.IsDisowned()) {
            self.DisableDisowned();
            PROVIDERS::ForEach([&]<class P> {
               using PP = P;
               LglsVerbose(NameOf<PP>());
               self.PP::AllocateFresh(self.PP::RequestHeap(1));
            });
            return;
         }

         const bool reusable = PROVIDERS::ForEachAnd([&]<class P> {
            const auto a = self.template GetAllocation<P::Id::First>();
            //LglsAssumeDev(a, "Allocation should be valid");
            //LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
            return a and a->GetUses() == 1;
         });

         if (reusable) {
            // We don't deallocate the memory. We can reuse it.         
            // But we have to destroy all shared elements.              
            self.template Free<false>();
            if constexpr (CT::ContainsMany<C>) {
               PROVIDERS::ForEach([&]<class P> {
                  using PP = P;
                  LglsVerbose(NameOf<PP>());
                  self.PP::AllocateLess(1);
               });
            }
            return;
         }

         // If reached we have a guarantee, that elements are referenced
         // from elsewhere as well, so we can't afford to call any      
         // destructors. All we do is reset this container and allocate 
         // a new block, which will be exclusively ours.                
         self.Free();
         PROVIDERS::ForEach([&]<class P> {
            using PP = P;
            LglsVerbose(NameOf<PP>());
            self.PP::AllocateFresh(self.PP::RequestHeap(1));
         });
      }

      /// A helper for clearing and allocating memory before assignment.      
      /// Calls destructors on all elements, except the first one.            
      ///   @attention operates in all relevant dimensions simultaneously     
      ///   @return true if first element is valid and can be assigned to     
      template<CT::HeapAllocated C>
      bool PrepareForReassignment(this C& self) {
         self.PrepareForReconstruction(); //TODO temporary solution
         return false;
      }
      /*template<CT::HeapAllocated C> //TODO this is too complex to implement, because each dimension can be of different sparseness, so we have to assign to dimensions that are dense, but deep free dimensions that are sparse
      bool PrepareForReassignment(this C& self) {
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");

         using PROVIDERS = C::FindProviders(Id{});
         static_assert(not PROVIDERS::Empty);
         if (self.IsDisowned()) {
            self.DisableDisowned();
            PROVIDERS::ForEach([&]<class P> {
               self.P::AllocateFresh(self.P::RequestHeap(1));
            });
            return false;
         }

         const bool reusable = PROVIDERS::ForEachAnd([&]<class P> {
            const auto a = self.template GetAllocation<P::Id::First>();
            LglsAssumeDev(a, "Allocation should be valid");
            LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
            return a->GetUses() == 1;
         });

         if (reusable) {
            // We don't deallocate the memory. We can reuse it.         
            if constexpr (CT::ContainsMany<C>) {
               // But we have to destroy all trailing elements.         
               // Just make sure indirections are dereferenced for the  
               // first element, in case it's sparse.                   
               auto first = self.GetHandle();
               auto item = first + 1;
               auto const itemsEnd = first + self.GetCount();
               while (item.GetRaw() != itemsEnd.GetRaw()) {
                  item.template Free<false>();
                  ++item;
               }
               if_available(first.template Free<false>());
               if_available(first.ResetAllEntries());
            }
            else if (self.IsSparse()) {
               self.template Free<false>();
               if_available(self.ResetAllEntries());
            }
            return true;
         }

         // If reached we have a guarantee, that elements are referenced
         // from elsewhere as well, so we can't afford to call any      
         // destructors. All we do is reset this container and allocate 
         // a new block, which will be exclusively ours.                
         self.Free();
         PROVIDERS::ForEach([&]<class P> {
            self.P::AllocateFresh(self.P::RequestHeap(1));
         });
         return false;
      }*/
      
      /// Overwrite first element using an intent                             
      ///   @attention Assumes destination memory has been constructed,       
      ///      including all levels of indirection                            
      ///   @attention Does not modify any container state                    
      ///   @attention This overwrites previous entry without dereferencing   
      ///      it, and without destroying anything                            
      ///   @attention Works in one dimension at a time!                      
      ///   @param intent assignment argument. If this container              
      ///      is statically typed, this can be any assignment argument,      
      ///      otherwise it has to be an instance of the contained type.      
      template<Cid SID = ID, CT::Container C, CT::Intent I> requires Relevant<SID>
      void AssignWithIntent(this C&& self, I&& intent) {
         static_assert(CT::ContainsOne<C>,
            "Assigning only first element in a container with many. GetHandle() first?");
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");
         static_assert(not CT::Cloned<I> and not CT::Copied<I>,
            "Since this function assumes container has been preallocated, "
            "it makes no sense to clone or copy here "
            "- it should be handled outside this call."
         );

         using IT = Decvq<Deref<TypeOf<I>>>;
         LglsAssumeDev(self.template GetRaw<SID>(), "Invalid heap");
         LglsAssumeDev(self.template IsTyped<SID>(), "Invalid type");
         decltype(auto) rhs = LglsFwd(intent.what);
         
         if constexpr (CT::Handle<IT>) {
            // We're emplacing using a handle, which can be faster due  
            // to carrying allocation data with itself when sparse,     
            // instead of searching for it when having DeepOwnership    
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
               //                                                       
               // Either this container or the handle is type-erased    
               auto T = rhs.template GetType<SID>();
               LglsAssumeDev(self.template IsSame<SID>(T), "Type mismatch");
               void* const src = rhs.template GetRawVoid<SID>();
               void* const dst = self.template GetRawVoid<SID>();

               if constexpr (CT::Moved<I>)
                  T.GetMoveAssigner()(src, dst);
               else if constexpr (CT::Abandoned<I>)
                  T.GetAbandonAssigner()(src, dst);
               else if constexpr (CT::Referred<I>)
                  T.GetReferAssigner()(src, dst);
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownAssigner()(src, dst);
               else
                  static_assert(false, "Unrecognized intent");
            }
            else {
               //                                                       
               // Both sides are statically-typed and we can benefit    
               // from a lot of compile-time optimizations              
               if constexpr (CT::Typed<C, IT>)
                  static_assert(Same<TypeOf<C, SID>, TypeOf<IT>>, "Type mismatch");
               else
                  LglsAssumeDev(self.template IsSame<SID>(rhs), "Type mismatch");

               using T = Tif<CT::Typed<C>, TypeOf<C, SID>, TypeOf<IT>>;
               T* const dst = self.template GetRawAs<T, SID>();

               if constexpr (CT::Mutable<T> or not I::IsMoved())
                  IntentAssign(*dst, I::Nest(*rhs.template GetRawAs<T, SID>()));
               else
                  IntentAssign(*dst, Refer(*rhs.template GetRawAs<T, SID>()));
            }

            if_available(self.template EmplaceEntries<SID>(LglsFwd(intent)));
         }
         else {
            if constexpr (CT::TypeErased<C, IT>) {
               //                                                       
               // This container is type-erased                         
               LglsAssumeDev((self.template IsSame<IT, SID>()), "Type mismatch");
               auto T = self.template GetType<SID>();
               void* const src = const_cast<void*>(static_cast<const void*>(&rhs));
               void* const dst = self.template GetRawVoid<SID>();

               if constexpr (CT::Moved<I>)
                  T.GetMoveAssigner()(src, dst);
               else if constexpr (CT::Abandoned<I>)
                  T.GetAbandonAssigner()(src, dst);
               else if constexpr (CT::Referred<I>)
                  T.GetReferAssigner()(src, dst);
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownAssigner()(src, dst);
               else
                  static_assert(false, "Unrecognized intent");
            }
            else {
               //                                                       
               // This container is statically-typed                    
               if constexpr (CT::Typed<C>)
                  static_assert(Same<TypeOf<C, SID>, IT>, "Type mismatch");
               else
                  LglsAssumeDev((self.template IsSame<IT, SID>()), "Type mismatch");

               IT* const dst = self.template GetRawAs<IT, SID>();
               IntentAssign(*dst, LglsFwd(intent));
            }

            if_available(self.template EmplaceEntries<SID>(LglsFwd(intent)));
         }
      }

      /// Swap the first element. Gracefully handling transitions between     
      /// embedded and unembedded handles.                                    
      ///   @param rhs - right hand side                                      
      ///   @attention Assumes types are the same                             
      ///   @attention Assumes both sides are allocated and initialized       
      ///   @attention Works in one dimension at a time!                      
      template<Cid SID = ID, CT::ContainsOne C, CT::ContainsOne RHS> requires Relevant<SID>
      void SwapInner(this C&& self, RHS& rhs) /*requires CT::ContainsOne<C, RHS> */{
         /*static_assert(CT::ContainsOne<C, RHS>,
            "Swapping only first element in a container with many. GetHandle() first?");*/

         if constexpr (CT::TypeErased<C, RHS>) {
            auto T = self.template GetType<SID>();
            auto S = T.GetSize();

            if (T.IsSparse()) {
               uintptr_t tmp;
               memcpy(&tmp,                           self.template Get<void, SID>(),  S);
               memcpy(self.template Get<void, SID>(),  rhs.template Get<void, SID>(),  S);
               memcpy( rhs.template Get<void, SID>(), &tmp,                            S);

               if constexpr (requires {
                  self.template GetEntriesInner<SID>();
                   rhs.template GetEntriesInner<SID>();
               }) {
                  // Both entry arrays are available, just swap them    
                  auto lhs_entry = self.template GetEntriesInner<SID>();
                  auto rhs_entry =  rhs.template GetEntriesInner<SID>();
                  for (int i = 0; i < T.GetIndirections(); ++i) {
                     ::std::swap(*lhs_entry, *rhs_entry);
                     ++lhs_entry;
                     ++rhs_entry;
                  }
               }
               else if constexpr (requires { self.template GetEntriesInner<SID>(); }) {
                  // Left entry array is available, right is emergent   
                  // Find the entries and reference them if we have to  
                  auto lhs_entry = self.template GetEntriesInner<SID>();
                  for (int i = 0; i < T.GetIndirections(); ++i) {
                     ++lhs_entry;
                     TODO();
                  }
               }
               else if constexpr (requires { rhs.template GetEntriesInner<SID>(); }) {
                  // Right entry array is available, left is emergent   
                  // Find the entries and reference them if we have to  
                  auto rhs_entry = rhs.template GetEntriesInner<SID>();
                  for (int i = 0; i < T.GetIndirections(); ++i) {
                     ++rhs_entry;
                     TODO();
                  }
               }
            }
            else {
               TODO();
               /*T tmp{Abandon(self.template Get<T>())};
               self.DestroyElement();
               self.EmplaceWithIntent(Abandon(rhs));
               rhs.DestroyElement();
               rhs.EmplaceWithIntent(Abandon(tmp));*/
            }
         }
         else {
            using T = Tif<CT::TypeErased<C>, TypeOf<RHS, SID>, TypeOf<C, SID>>;
            T& lhs_item = *self.template Get<T, SID>();
            T& rhs_item =  *rhs.template Get<T, SID>();

            if constexpr (CT::Sparse<T>) {
               ::std::swap(lhs_item, rhs_item);

               if constexpr (requires {
                  self.template GetEntriesInner<SID>();
                   rhs.template GetEntriesInner<SID>();
               }) {
                  // Both entry arrays are available, just swap them    
                  auto lhs_entry = DecvqAllCast(self.template GetEntriesInner<SID>());
                  auto rhs_entry = DecvqAllCast( rhs.template GetEntriesInner<SID>());
                  ForEachIndirection<T>([&lhs_entry, &rhs_entry] {
                     ::std::swap(*lhs_entry, *rhs_entry);
                     ++lhs_entry;
                     ++rhs_entry;
                  });
               }
               else if constexpr (requires { self.template GetEntriesInner<SID>(); }) {
                  // Left entry array is available, right is emergent   
                  // Find the entries and reference them if we have to  
                  auto lhs_entry = DecvqAllCast(self.template GetEntriesInner<SID>());
                  ForEachIndirection<T>([&lhs_entry] {
                     ++lhs_entry;
                     TODO();
                  });
               }
               else if constexpr (requires { rhs.template GetEntriesInner<SID>(); }) {
                  // Right entry array is available, left is emergent   
                  // Find the entries and reference them if we have to  
                  auto rhs_entry = DecvqAllCast(rhs.template GetEntriesInner<SID>());
                  ForEachIndirection<T>([&rhs_entry] {
                     ++rhs_entry;
                     TODO();
                  });
               }
            }
            else {
               if constexpr (requires { T {Abandon(lhs_item)}; }) {
                  // Abandon semantics are most optimal                 
                  T tmp {Abandon(lhs_item)};
                  new (&lhs_item) T {Abandon(rhs_item)};
                  new (&rhs_item) T {Abandon(tmp)};
               }
               else if constexpr (requires { lhs_item = LglsMov(rhs_item); }) {
                  // Move-assignment is second best                     
                  T tmp {LglsMov(lhs_item)};
                  lhs_item = LglsMov(rhs_item);
                  rhs_item = LglsMov(tmp);
               }
               else {
                  // Fallback to move-construction: requires destroyng  
                  // each item before reconstructing it in place.       
                  T tmp {LglsMov(lhs_item)};
                  lhs_item.~T();
                  new (&lhs_item) T {LglsMov(rhs_item)};
                  rhs_item.~T();
                  new (&rhs_item) T {LglsMov(tmp)};
               }
            }
         }
      }
   };
}
