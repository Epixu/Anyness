///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
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
      ///   @tparam C - the container                                         
      ///   @tparam A - the argument to test                                  
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
   /// Implements assignment for containers                                   
   ///   @tparam ID - heap we're removing from                                
   template<unsigned ID>
   struct Assignment {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      template<CT::Container C, class A>
      void Fill(this C&, A&&) requires CT::RangeAssignable<C, A>;
      
      /// Assign a value to the first element, if that element is initialized.
      /// If the element isn't initialized yet it will be constructed.        
      ///   @param argument - the argument to assign                          
      ///   @return reference to self                                         
      template<CT::Container C, class A>
      C& operator = (this C& self, A&& argument) requires CT::RangeAssignable<C, A> {
         if constexpr (not CT::HeapAllocated<C>) {
            // This container is on the stack, and by extension         
            // statically-typed and always initialized                  
            auto& data = self.template AccessStackById<ID>();
            data = FWD(argument);       
         }
         else {
            // This container is heap-allocated                         
            using T = Tif<CT::TypeErased<C>, A, TypeOf<C>>;
            if constexpr (CT::TypeErased<C>)
               LglsAssert(self.template IsSimilar<A>(), "Type mismatch");

            if (self.IsEmpty()) {
               // Container is empty, we might have to fresh-allocate   
               if constexpr (CT::UnfoldConstructible<T, A&&>) {
                  // Just construct the first element                   
                  self.PrepareForReconstruction();
                  self.EmplaceWithIntent(FWDIntent(argument));
               }
               else static_assert(false, "T can't be reconstructed");
            }
            else {
               // Container has at least one element                    
               if constexpr (CT::UnfoldAssignable<T, A&&>) {
                  // Reduce to one item and reassign if possible        
                  if (self.PrepareForReassignment())
                     self.AssignWithIntent(FWDIntent(argument));
                  else
                     self.EmplaceWithIntent(FWDIntent(argument));
               }
               else if constexpr (CT::UnfoldConstructible<T, A&&>) {
                  // Assignment isn't available for T - destroy all     
                  // items and reconstruct the first one                
                  self.PrepareForReconstruction();
                  self.EmplaceWithIntent(FWDIntent(argument));
               }
               else static_assert(false, "T can't be reassigned or reconstructed");
            }

            if_available(self.SetCountInner(1));
         }
         
         return self;
      }

   protected:
      /// A helper for clearing and allocating memory before construction.    
      /// Calls destructors on all elements, if any were initialized.         
      template<CT::HeapAllocated C>
      void PrepareForReconstruction(this C& self) {
         // 1. We free if we have to                                    
         auto& a = self.GetAllocationInner();
         if (a) {
            LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");

            if (a->GetUses() == 1) {
               // We don't deallocate the memory - we can reuse it      
               if_available(self.FreeDeep());

               if constexpr (CT::ContainsMany<C>)
                  self.AllocateLess(1);
            }
            else {
               // Notice that no element will be destroyed, because in  
               // this case we have a guarantee, that elements are      
               // referenced from elsewhere as well                     
               if_available(self.template FreeDeep<false>());

               // Dereference memory and reset state                    
               a->Free();
               a = nullptr;
            }
         }

         // 2. We allocate if we have to                                
         if (not a)
            self.AllocateFresh(self.RequestHeap(1));
      }

      /// A helper for clearing and allocating memory before assignment.      
      /// Calls destructors on all elements, except the first one.            
      ///   @return true if first element is valid and can be assigned to     
      template<CT::HeapAllocated C>
      bool PrepareForReassignment(this C& self) {
         // 1. We free if we have to                                    
         auto& a = self.GetAllocationInner();
         if (a) {
            LglsAssumeDev(a->GetUses() >= 1, "Bad memory dereferencing");

            if (a->GetUses() == 1) {
               // We don't deallocate the memory - we can reuse it      
               if constexpr (CT::ContainsMany<C>)
                  self.SelectInner(1, self.GetCount() - 1).FreeInner();
               return true;
            }

            // Notice that no element will be destroyed, because in     
            // this case we have a guarantee, that elements are         
            // referenced from elsewhere as well                        
            if_available(self.template FreeDeep<false>());
            
            // Dereference memory and reset state                       
            a->Free();
            a = nullptr;
         }

         // 2. We allocate if we have to                                
         if (not a)
            self.AllocateFresh(self.RequestHeap(1));
         return false;
      }
      
      /// Overwrite first element using an intent                             
      ///   @attention assumes destination memory has been constructed,       
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @param intent - assignment argument. If this container            
      ///      is statically typed, this can be any assignment argument,      
      ///      otherwise it has to be an instance of the contained type.      
      template<CT::Container C, CT::Intent I>
      void AssignWithIntent(this C& self, I&& intent) {
         using IT = Decvq<Deref<TypeOf<I>>>;
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");
         decltype(auto) rhs = FWD(intent.what);

         if constexpr (CT::Handle<IT>) {
            // We're emplacing using a handle, which can be faster due  
            // to carrying allocation data with itself when sparse,     
            // instead of searching for it when having DeepOwnership    
            if constexpr (C::TypeErased or IT::TypeErased) {
               //                                                       
               // Either this container or the handle is type-erased    
               auto T = rhs.GetTypeInner();
               LglsAssumeDev(self.IsSimilar(T), "Type mismatch");

               const auto src = const_cast<void*>(rhs.GetRaw());
               const auto dst = self.template AccessStackById<ID>();
               if constexpr (CT::Moved<I>)
                  T.GetMoveAssigner()(dst, src);
               else if constexpr (CT::Abandoned<I>)
                  T.GetAbandonAssigner()(dst, src);
               else if constexpr (CT::Referred<I>)
                  T.GetReferAssigner()(dst, src);
               else if constexpr (CT::Copied<I>)
                  T.GetCopyAssigner()(dst, src);
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownAssigner()(dst, src);
               else if constexpr (CT::Cloned<I>)
                  T.GetCloneAssigner()(dst, src);
               else
                  static_assert(false, "Unrecognized intent");

               if constexpr (CT::DeeplyOwned<C>) {
                  if constexpr (I::IsKept())
                     *self.GetEntries() = *rhs.GetEntries();
                  else
                     *self.GetEntries() = nullptr;
                  self.KeepDeep();
               }
            }
            else {
               //                                                       
               // Both sides are statically-typed and we can benefit    
               // from a lot of compile-time optimizations              
               using T = TypeOf<C>;
               static_assert(CT::Similar<T, TypeOf<IT>>, "Type mismatch");
               T* data = static_cast<T*>(self.template AccessStackById<ID>());
               IntentAssign(*data, I::Nest(*rhs.GetRaw()));
            }
         }
         else if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            LglsAssumeDev(CT::Dense<IT>, "Sparseness mismatch");
            LglsAssumeDev(self.template IsSimilar<IT>(), "Type mismatch");
            auto T = self.GetTypeInner();

            const auto src = const_cast<void*>(static_cast<const void*>(&rhs));
            const auto dst = self.template AccessStackById<ID>();
            if constexpr (CT::Moved<I>)
               T.GetMoveAssigner()(dst, src);
            else if constexpr (CT::Abandoned<I>)
               T.GetAbandonAssigner()(dst, src);
            else if constexpr (CT::Referred<I>)
               T.GetReferAssigner()(dst, src);
            else if constexpr (CT::Copied<I>)
               T.GetCopyAssigner()(dst, src);
            else if constexpr (CT::Disowned<I>)
               T.GetDisownAssigner()(dst, src);
            else if constexpr (CT::Cloned<I>)
               T.GetCloneAssigner()(dst, src);
            else
               static_assert(false, "Unrecognized intent");
         }
         else {
            //                                                          
            // This container is statically-typed                       
            using T = TypeOf<C>;
            static_assert(CT::Similar<T, IT>, "Type mismatch");
            T* data = static_cast<T*>(self.template AccessStackById<ID>());
            IntentAssign(*data, FWD(intent));
         }
      }
   };
}
