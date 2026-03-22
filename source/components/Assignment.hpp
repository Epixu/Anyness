///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Iteration-Range.hpp"
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
               // Generally, shallow intents are always supported,      
               // but copying will call element assigners, so we        
               // have to check if the contained type supports it       
               if constexpr (Copied<SA>)
                  return ReferAssignable<T>;
               else
                  return true;
            }
            else {
               // Cloning always calls element assigners, and we        
               // have to check whether contained elements can do it    
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
   template<Cid ID>
   struct Assignment {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      //template<CT::Container C, class A>
      //void Fill(this C&, A&&) requires CT::RangeAssignable<C, A>;
      
      /// Assign a value to the first element, if that element is initialized.
      /// If the element isn't initialized yet it will be constructed.        
      ///   @param argument the argument to assign                            
      ///   @return reference to self                                         
      template<CT::Container C, class A>
      C& Assign(this C& self, A&& argument)
      requires (CT::RangeAssignable<C, A> /*and CT::Contiguous<C>*/) {
         if constexpr (not CT::Contiguous<C>) {
            // Assignment for maps/sets falls back to insert/merge      
            self.Clear();
                 if_available(self.Insert(LglsFwd(argument)))
            else if_available(self.Merge (LglsFwd(argument)))
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

                  if constexpr (CT::Copied<IntentOf(argument)>)
                     self.GetHandle().EmplaceWithIntent(Refer(LglsFwd(argument)));
                  else
                     self.GetHandle().EmplaceWithIntent(FWDIntent(argument));
               }
               else static_assert(false, "T can't be reconstructed");
            }
            else {
               // Container has at least one element                    
               if constexpr (not CT::Cloned<IntentOf(argument)> and CT::UnfoldAssignable<T, A&&>) {
                  // Reduce to one item and reassign if possible        
                  if (self.PrepareForReassignment()) {
                     if constexpr (CT::Copied<IntentOf(argument)>)
                        self.GetHandle().AssignWithIntent(Refer(LglsFwd(argument)));
                     else
                        self.GetHandle().AssignWithIntent(FWDIntent(argument));
                  }
                  else {
                     if constexpr (CT::Copied<IntentOf(argument)>)
                        self.GetHandle().EmplaceWithIntent(Refer(LglsFwd(argument)));
                     else
                        self.GetHandle().EmplaceWithIntent(FWDIntent(argument));
                  }
               }
               else if constexpr (CT::UnfoldConstructible<T, A&&>) {
                  // Assignment isn't available for T - destroy all     
                  // items and reconstruct the first one                
                  self.PrepareForReconstruction();

                  if constexpr (CT::Copied<IntentOf(argument)>)
                     self.GetHandle().EmplaceWithIntent(Refer(LglsFwd(argument)));
                  else
                     self.GetHandle().EmplaceWithIntent(FWDIntent(argument));
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
      friend struct Conversion;

      /// A helper for clearing and allocating memory before construction.    
      /// Calls destructors on all elements, if any were initialized.         
      template<CT::HeapAllocated C>
      void PrepareForReconstruction(this C& self) {
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");
         auto& a = self.GetAllocationInner();
         if (not a) {
            // Nothing was allocated                                    
            self.AllocateFresh(self.RequestHeap(1));
            return;
         }

         // If reached, then we have to free previous elements          
         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
         if (a->GetUses() == 1) {
            // We don't deallocate the memory - we can reuse it         
            // But we have to destroy all elements                      
            self.DestroyAllElements();
            if constexpr (CT::ContainsMany<C>)
               self.AllocateLess(1);
            return;
         }

         // If reached we have a guarantee, that elements are           
         // referenced from elsewhere as well, so we can't afford to    
         // call any destructors. All we do is reset this container and 
         // allocate a new block, which will be exclusively ours.       
         self.Destroy();
         self.AllocateFresh(self.RequestHeap(1));
      }

      /// A helper for clearing and allocating memory before assignment.      
      /// Calls destructors on all elements, except the first one.            
      ///   @return true if first element is valid and can be assigned to     
      template<CT::HeapAllocated C>
      bool PrepareForReassignment(this C& self) {
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");

         auto& a = self.GetAllocationInner();
         if (not a) {
            // Nothing was allocated                                    
            self.AllocateFresh(self.RequestHeap(1));
            return false;
         }
         
         // If reached, then we have to free previous elements          
         LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");
         if (a->GetUses() == 1) {
            // We don't deallocate the memory - we can reuse it         
            if constexpr (CT::ContainsMany<C>) {
               // But we have to destroy all trailing elements          
               // Just make sure indirections are dereferenced          
               // for the first element, in case it's sparse            
               auto first = self.GetHandle();
               auto item = first + (self.IsSparse() ? 0 : 1);
               auto const itemsEnd = first + self.GetCount();
               while (item.GetRaw() != itemsEnd.GetRaw()) {
                  item.DestroyElement();
                  ++item;
               }
               if_available(first.ResetEntries());
            }
            else if (self.IsSparse()) {
               self.DestroyElement();
               if_available(self.ResetEntries());
            }
            return true;
         }

         // If reached we have a guarantee, that elements are           
         // referenced from elsewhere as well, so we can't afford to    
         // call any destructors. All we do is reset this container and 
         // allocate a new block, which will be exclusively ours.       
         self.Destroy();
         self.AllocateFresh(self.RequestHeap(1));
         return false;
      }
      
      /// Overwrite first element using an intent                             
      ///   @attention assumes destination memory has been constructed,       
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @attention this overwrites previous entry without dereferencing   
      ///      it, and without destroying anything                            
      ///   @param intent assignment argument. If this container              
      ///      is statically typed, this can be any assignment argument,      
      ///      otherwise it has to be an instance of the contained type.      
      template<CT::Container C, CT::Intent I>
      void AssignWithIntent(this C&& self, I&& intent) {
         static_assert(CT::ContainsOne<C>,
            "Assigning only first element in a container with many. GetHandle() first?");
         static_assert(CT::Contiguous<C>,
             "Can be used only for contiguous containers");
         using IT = Decvq<Deref<TypeOf<I>>>;
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");
         decltype(auto) rhs = LglsFwd(intent.what);
         static_assert(not CT::Cloned<I> and not CT::Copied<I>,
            "Since this function assumes container has been preallocated, "
            "it makes no sense to clone or copy here "
            "- it should be handled outside this call."
         );
         
         if constexpr (CT::Handle<IT>) {
            // We're emplacing using a handle, which can be faster due  
            // to carrying allocation data with itself when sparse,     
            // instead of searching for it when having DeepOwnership    
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
               //                                                       
               // Either this container or the handle is type-erased    
               auto T = rhs.GetTypeInner();
               LglsAssumeDev(self.IsSame(T), "Type mismatch");
               const auto src = const_cast<void*>(rhs.GetRaw());
               const auto dst = self.template AccessProvider<ID>();

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

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
            else {
               //                                                       
               // Both sides are statically-typed and we can benefit    
               // from a lot of compile-time optimizations              
               using T = TypeOf<C>;
               static_assert(Same<T, TypeOf<IT>>, "Type mismatch");
               T* data = static_cast<T*>(self.template AccessProvider<ID>());
               IntentAssign(*data, I::Nest(*rhs.GetRaw()));

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
         }
         else {
            if constexpr (CT::TypeErased<C>) {
               //                                                       
               // This container is type-erased                         
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
               auto T = self.GetTypeInner();
               const auto src = const_cast<void*>(static_cast<const void*>(&rhs));
               const auto dst = self.template AccessProvider<ID>();

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

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
            else {
               //                                                       
               // This container is statically-typed                    
               using T = TypeOf<C>;
               static_assert(Same<T, IT>, "Type mismatch");
               T* data = static_cast<T*>(self.template AccessProvider<ID>());
               IntentAssign(*data, LglsFwd(intent));

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
         }
      }

      /// Swap contents. Gracefully handling transitions between embedded     
      /// and unembedded handles.                                             
      ///   @param rhs - right hand side                                      
      ///   @attention assumes types are the same                             
      ///   @attention assumes both sides are allocated and initialized       
      template<CT::Container C, CT::Container RHS>
      void SwapInner(this C&& self, RHS& rhs) requires CT::ContainsOne<C, RHS> {
         static_assert(CT::ContainsOne<C, RHS>,
            "Swapping only first element in a container with many. GetHandle() first?");

         if constexpr (CT::TypeErased<C, RHS>) {
            auto T = self.GetType();
            auto S = T.GetSize();

            if (T.IsSparse()) {
               uintptr_t tmp;
               memcpy(&tmp,       self.Get(), S);
               memcpy(self.Get(), rhs.Get(),  S);
               memcpy(rhs.Get(),  &tmp,       S);

               auto lhs_entry = self.GetEntries();
               auto rhs_entry = rhs.GetEntries();
               for (int i = 0; i < T.GetIndirections(); ++i) {
                  ::std::swap(*lhs_entry, *rhs_entry);
                  ++lhs_entry;
                  ++rhs_entry;
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
            using T = Tif<CT::TypeErased<C>, TypeOf<RHS>, TypeOf<C>>;
            T& lhs_item = self.template Get<T>();
            T& rhs_item = rhs.template Get<T>();

            if constexpr (CT::Sparse<T>) {
               ::std::swap(lhs_item, rhs_item);

               auto lhs_entry = DecvqAllCast(self.GetEntries());
               auto rhs_entry = DecvqAllCast( rhs.GetEntries());
               ForEachIndirection<T>([&lhs_entry, &rhs_entry] {
                  ::std::swap(*lhs_entry, *rhs_entry);
                  ++lhs_entry;
                  ++rhs_entry;
               });
            }
            else {
               if constexpr (requires { T {Abandon(lhs_item)}; }) {
                  T tmp{Abandon(lhs_item)};
                  lhs_item.~T();
                  new (&lhs_item) T{Abandon(rhs_item)};
                  rhs_item.~T();
                  new (&rhs_item) T{Abandon(tmp)};
               }
               else {
                  T tmp{LglsMov(lhs_item)};
                  lhs_item.~T();
                  new (&lhs_item) T{LglsMov(rhs_item)};
                  rhs_item.~T();
                  new (&rhs_item) T{LglsMov(tmp)};
               }
            }
         }
      }
   };
}
