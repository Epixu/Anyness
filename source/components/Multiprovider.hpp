///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
LglsDisableWarningPush
LglsDisableWarning_UnusedLocalTypedef


namespace Langulus::Anyness::Component
{
   template<class...> struct Multiprovider;

   template<CT::Component...TN> requires (CountEnabled<TN...> == 0)
   struct Multiprovider<TN...> {
      using CTTI_Component = Yes<>;
      static constexpr bool SkipThisComponent = true;
   };

   ///                                                                        
   /// Combines multiple heap/stack components into a unified interface to    
   /// combat C++ base method ambiguities, and to add a bit more convenience. 
   ///   @tparam TC0, TC1, TCN... all the heap/stack components to unify      
   template<CT::Component...TN> requires (CountEnabled<TN...> >= 2)
   struct LANGULUS_EBCO Multiprovider<TN...> : TN... {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C> static { return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C> static { return typename C::Id{}; }));

      static_assert(Subcomponents::ForEachIndexedAnd([]<class C, size_t I> {
         return C::Id::Count == 1 and C::Id::First == I; }),
         "Each enabled subcomponent needs to be dedicated to their single dimension, "
         "and all subcomponents need to be sequential"
      );

      static constexpr int ComponentPrecedence = -2000;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::ComponentPrecedence == -2000; }),
         "All precedences should match");

      static constexpr bool HeapCanBeNull = Subcomponents::ForEachOr([]<class C> { return C::HeapCanBeNull; });

      #define if_inherits(...) requires (Subcomponents::ForEachOr([&]<class C> { \
         return requires { self.C::__VA_ARGS__; }; }))

      /// Get a direct access to the heap memory                              
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<Cid SID = 0>
      constexpr auto GetRaw(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetRaw();
      }
      
      /// Get a direct access to the heap memory as a different type          
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<class T, Cid SID = 0>
      constexpr auto GetRawAs(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template GetRawAs<T>();
      }

      /// Get a direct access to the initialized heap memory's end.           
      ///   @attention this makes sense only when provider is contiguous.     
      template<Cid SID = 0>
      constexpr auto GetRawEnd(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetRawEnd();
      }
    
      /// Get a direct access to the entire heap reserve's end.               
      template<Cid SID = 0>
      constexpr auto GetRawReserveEnd(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetRawReserveEnd();
      }
      
      /// Get pointer to the first element for the given dimension.           
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention no type-safety                                         
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container has valid memory                 
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the container, if statically typed                     
      ///   @tparam SID can be used to access specific dimension              
      ///   @return pointer to the first element of the desired dimension     
      template<class AS = void, Cid SID = 0>
      auto* Get(this auto&& self) assumptious {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template Get<AS>();
      }

      /// Get first element as a handle, or any desired wrapping type.        
      /// Conversion or copying may occur, depending on type.                 
      ///   @attention will throw if incompatible type is provided            
      ///   @tparam AS the type we're wrapping in                             
      ///   @tparam SID can be used to access specific dimension              
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, Cid SID = 0>
      decltype(auto) As(this auto&& self) {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template As<AS>();
      }

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type. Available only if container has DeepType.   
      ///   @return the most concrete representation of the first item        
      template<Cid SID = 0, class AS = void>
      auto GetResolved(this auto&& self) {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template GetResolved<SID, AS>();
      }

      /// Get first element, removing 'count' indirections                    
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @tparam SID can be used to access specific dimension              
      ///   @tparam AS specify the type we wrap the result in.                
      ///      Using 'void' will default to C::DeepType.                      
      ///   @param count how many levels of indirection to remove?            
      ///   @return the dense first element for chosen dimension              
      template<Cid SID = 0, class AS = void>
      auto GetDense(this auto&& self, size_t count = -1) {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template GetDense<SID, AS>(count);
      }

   protected:
      LglsComIterationOperators(friend);
      LglsComReserveEmergent(friend);
      LglsComInsertion(friend);
      LglsComMerging(friend);
      LglsComEmplacement(friend);
      LglsComConversion(friend);
      LglsComOwnershipEmergent(friend);
      
      /// Get the heap pointer (inner)                                        
      template<Cid SID = 0>
      constexpr auto& GetHeapInner(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetHeapInner();
      }

      /// Get a direct access to the heap memory                              
      ///   @attention using raw pointer while self.IsEmpty() may lead to     
      ///      undefined behavior                                             
      template<Cid SID = 0>
      constexpr void* GetRawVoid(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetRawVoid();
      }

      /// Set the heap pointer, any data pointer will do                      
      template<Cid SID = 0>
      constexpr void SetHeapInner(this auto& self, CT::Sparse auto heap) assumptious {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetHeapInner(heap);
      }

      /// Reset the heap pointer to null                                      
      template<Cid SID = 0>
      constexpr void SetHeapInner(this auto& self, nullptr_t) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetHeapInner(nullptr);
      }
      
      /// Get a size based on reflected allocation page and count.            
      /// This will allocate memory for relevant headers, footers, and types  
      /// across all dimensions used in this heap component.                  
      ///   @param reserve the number of elements to request                  
      template<Cid SID = 0>
      constexpr auto RequestHeap(this auto const& self, size_t reserve) assumptious
      -> Request if_inherits(RequestHeap(reserve)) {
         using C = typename Subcomponents::template At<SID>;
         return self.C::RequestHeap(reserve);
      }

      /// Default-initialize the heap pointer                                 
      constexpr void ConstructDefault(this auto& self) noexcept {
         Subcomponents::ForEach([&]<class C> noexcept {
            using T = Decay<C>;
            self.T::ConstructDefault();
         });
      }
      
      /// Transfer from any kind of container, respecting intents             
      ///   @param intent The intent and container to transfer from.          
      ///   @param reserve Optional reserve override, which is taken into     
      ///      account only when we're cloning or copying, as only then       
      ///      a new allocation occurs.                                       
      void ConstructFrom(this auto& self, auto&& intent, size_t reserve = 0) {
         Subcomponents::ForEach([&]<class C> {
            self.C::ConstructFrom(LglsFwd(intent), reserve);
         });
      }

      /// Free this container and absorb from any other, respecting intents   
      ///   @param intent the intent and container to assign from             
      /*void AssignFrom(this auto& self, auto&& intent) {
         Subcomponents::ForEach([&]<class C> {
            self.C::AssignFrom(LglsFwd(intent));
         });
      }*/
      
      /// Allocate a fresh allocation                                         
      ///   @attention changes allocation, heap pointer and reserve count only
      ///   @param request request to fulfill                                 
      template<Cid SID = 0>
      void AllocateFresh(this auto& self, const Request& request) {
         using C = typename Subcomponents::template At<SID>;
         self.C::AllocateFresh(request);
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes container is typed                             
      ///   @param elements number of elements to allocate                    
      template<Cid SID = 0>
      void AllocateMore(this auto& self, size_t elements) {
         using C = typename Subcomponents::template At<SID>;
         self.C::AllocateMore(elements);
      }

      /// Shrink the block, depending on currently reserved	elements.         
      /// Initialized elements on the back will be destroyed.                 
      /// When MANAGED_MEMORY is enabled we have a strong guarantee that      
      /// allocations never move when shrinking.                              
      ///   @param elements number of elements to reserve                     
      template<Cid SID = 0>
      void AllocateLess(this auto& self, size_t elements) {
         using C = typename Subcomponents::template At<SID>;
         self.C::AllocateLess(elements);
      }

      /// Remap footer requests onto the new reserve                          
      ///   @param elements the newly reserved number of elements             
      ///   @attention works on one dimension at a time!                      
      template<Cid SID = 0>
      void RemapHeapRequests(this auto& self, size_t elements) {
         using C = typename Subcomponents::template At<SID>;
         self.C::RemapHeapRequests(elements);
      }

      /// Invoked to remedy the situation when element constructors throw     
      ///   @param n the number of elements that were actually initialized    
      template<Cid SID = 0>
      void PartialSuccess(this auto& self, size_t n) {
         using C = typename Subcomponents::template At<SID>;
         self.C::PartialSuccess(n);
      }

      /// Branch out the current container by doing a shallow copy.           
      /// Happens when you try to modify a container with strong ownership    
      /// from somewhere else (when GetUses() > 1). Allocates a fresh         
      /// allocation in the case we haven't allocated anything yet.           
      /// Essentially implements the Copy-On-Write principle.                 
      ///   @param elements usually branching is accompanied by a resize,     
      ///      so specify it here                                             
      template<Cid SID = 0>
      void BranchOut(this auto& self, size_t elements) {
         using C = typename Subcomponents::template At<SID>;
         self.C::BranchOut(elements);
      }
      
      /// Destroys only the first element.                                    
      ///   @attention destroys one dimension at a time!                      
      ///   @tparam FORCE_DESTROY set to 'false' to only dereference.         
      ///      It will still destroy the element, but only when fully         
      ///      dereferenced in all its indirections.                          
      template<bool FORCE_DESTROY = true, Cid SID = 0>
      void DestroyElement(this auto& self) assumptious {
         using C = typename Subcomponents::template At<SID>;
         self.C::template DestroyElement<FORCE_DESTROY>();
      }

      /// Destroys all elements.                                              
      ///   @attention destroys one dimension at a time!                      
      ///   @tparam FORCE_DESTROY set to 'false' to only dereference.         
      ///      It will still destroy the element, but only when fully         
      ///      dereferenced in all its indirections.                          
      template<bool FORCE_DESTROY = true, Cid SID = 0>
      void DestroyAllElements(this auto& self) assumptious {
         using C = typename Subcomponents::template At<SID>;
         self.C::template DestroyAllElements<FORCE_DESTROY>();
      }

      #undef if_inherits
   };
}

LglsDisableWarningPop
