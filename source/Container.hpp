///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Component.hpp"
#include "components/State-Stack.hpp"
#include <Langulus/IntentOf.hpp>
#include <Langulus/Sequence.hpp>
#include <Langulus/HashOf.hpp>
#include <Langulus/CT/Bool.hpp>

/// Make the rest of the code aware, that Langulus::Anyness has been included 
#define LANGULUS_LIBRARY_ANYNESS() 1
#define LANGULUS_ANYNESS_VERBOSITY_MASTER_SWITCH() 0

#define if_available(...) if constexpr (requires { __VA_ARGS__; }) { __VA_ARGS__; }


namespace Langulus::Anyness
{
   struct Handle;
   struct HandleMut;
   struct HandleDisowned;
   struct HandleDisownedMut;
   template<class> struct THandle;
   template<class> struct THandleEmergent;
   template<class> struct THandleDisowned;
   template<CT::Handle, CT::Handle> struct THandlePair;

   struct Any;
   struct Bytes;
   struct Construct;
   struct Many;
   struct Neat;
   struct Pair;
   struct Path;
   struct Text;
   struct Tag;

   template<CT::NotVoid> struct TOwn;
   template<class>       struct TRef;
   template<CT::NotVoid> struct TTag;
   template<CT::NotVoid> struct TAny;
   template<CT::NotVoid> struct THive;
   template<CT::NotVoid> struct TMany;

   template<CT::NotVoid,              StateValue SORT = StateValue::Variable> struct TSet;
   template<CT::NotVoid, CT::NotVoid, StateValue SORT = StateValue::Variable> struct TMap;
   template<CT::NotVoid, CT::NotVoid> struct TPair;

   namespace Inner
   {
      template<StateValue SORT = StateValue::Variable> struct Map;
      template<StateValue SORT = StateValue::Variable> struct Set;

      /// Tag for calling container constructors that initalize the           
      /// internal stack tuple. Extensively used by handles and iterators.    
      struct Stackwise {};

      /// Tag for calling container constructors that emplace elements.       
      /// Often used to disambiguate and state clear intent.                  
      struct Piecewise {};

      /// Tag for calling container constructors that absorb container.       
      /// Often used to disambiguate and state clear intent.                  
      struct Absorb {};

      /// Inner function that picks the best possible handle type, depending  
      /// on a container's constness and type-erasedness, as well as member   
      /// types HandleType and HandleMutType. Guarantees to always result in  
      /// a handle. No-op if C is already a handle.                           
      template<CT::Container C> 
      consteval auto DecideHandleType() {
         static_assert(not CT::Sheddable<C>, "Strip sheddables first");
         static_assert(not CT::Reference<C>, "Strip references first");

         if constexpr (CT::Handle<C>) {
            // No-op                                                    
            return Types<C> {};
         }
         else if constexpr (requires {typename C::HandleType; typename C::HandleMutType; }) {
            // Always prioritize custom handle types if defined         
            return Types<Tmut<C, typename C::HandleMutType, typename C::HandleType>> {};
         }
         else if constexpr (CT::TypeErased<C>) {
            // Type-erased handle                                       
            if constexpr (CT::Owned<C>)
               return Types<Tmut<C, HandleMut,         Handle>> {};
            else
               return Types<Tmut<C, HandleDisownedMut, HandleDisowned>> {};
         }
         else {
            // Statically-typed handle                                  
            using T     = TypeOf<C>;
            using Inner = Tmut<C, T&, ConstAll<T&>>;
            if constexpr (CT::Owned<C>)
               return Types<THandle        <Inner>> {};
            else
               return Types<THandleDisowned<Inner>> {};
         }
      }

      /// Inner function that picks the best possible handle or reference     
      /// type, depending on a container's constness and type-erasedness, as  
      /// well as member types HandleType and HandleMutType. Unlike           
      /// DecideHandleType, this one will prefer to use references whenever   
      /// possible.                                                           
      template<CT::Container C> 
      consteval auto DecidePickType() {
         static_assert(not CT::Sheddable<C>, "Strip sheddables first");
         static_assert(not CT::Reference<C>, "Strip references first");

         if constexpr (requires {typename C::Pick; typename C::PickMut; }) {
            // Always prioritize custom pick types if defined           
            return Types<Tmut<C, typename C::PickMut, typename C::Pick>> {};
         }
         else if constexpr (CT::TypeErased<C>) {
            // Type-erased containers always result in handle picks     
            return DecideHandleType<C>();
         }
         else {
            // Statically-typed container - always prefer a reference,  
            // unless we're referencing an owned pointer                
            using T = TypeOf<C>;
            if constexpr (CT::Owned<C> and CT::Sparse<T> and CT::Mutable<C>)
               return DecideHandleType<C>();
            else
               return Types<ConstAll<T&>> {};
         }
      }
   }

   constexpr Inner::Stackwise Stackwise {};
   constexpr Inner::Piecewise Piecewise {};
   constexpr Inner::Absorb    Absorb {};
   
   template<CT::Container C>
   using DecideHandle = typename decltype(Inner::DecideHandleType<Deref<C>>())::First;

   template<CT::Container C>
   using DecidePick = typename decltype(Inner::DecidePickType<Deref<C>>())::First;

   namespace Component
   {
   ///                                                                        
   /// A container definition using composition                               
   ///   @tparam COMPONENTS list of components that define the container      
   ///      behavior. Order is verified based on ComponentPrecedence members  
   ///      for various reasons, the main ones being initialization order and 
   ///      build-time optimization: too many superficially different template
   ///      specializations will bloat code generation significantly and slow 
   ///      builds down a lot...                                              
   template<CT::Component...COMPONENTS>
   requires ValidComponentOrder<COMPONENTS...>
   struct LANGULUS_EBCO Container : COMPONENTS..., DecideStateComponent<COMPONENTS...> {
      using CTTI_Container = Yes<>;
      using ComponentList = Types<COMPONENTS..., DecideStateComponent<COMPONENTS...>>;
      using Base = Container;

      /// Generate a new container type with additional components            
      ///   @attention doesn't check for duplicates                           
      template<CT::Component...MORE_COMPONENTS>
      using Include = Container<COMPONENTS..., MORE_COMPONENTS...>;

      /// Check if container is valid                                         
      constexpr bool IsValid() const noexcept {
         if (this->GetCount() > 0)
            return true;

         bool for_other_reasons = false;
         ComponentList::ForEach([this, &for_other_reasons]<class C>{
            if_available(for_other_reasons |= this->C::IsValid());
         });
         return for_other_reasons;
      }
      
      /// Check if a component is included at compile-time                    
      template<class C>
      static constexpr bool HasComponent
         = AkinAsOneOf<C, COMPONENTS..., DecideStateComponent<COMPONENTS...>>;

      /// Get the number of heap providers (all dimensions)                   
      /// Needs to be public, because it's used in concept checks.            
      static consteval size_t CountHeapProviders() {
         size_t count = 0;
         ComponentList::ForEach([&count]<class C> {
            if constexpr (requires { typename C::HeapProvider; })
               ++count;
         });
         return count;
      }

   protected:
      LglsComIterationOperators(friend);
      LglsComTypedStack(friend);
      LglsComStack(friend);
      LglsComHeapReference(friend);
      LglsComHeapMovable(friend);
      LglsComOwnershipEmergent(friend);
      LglsComOwnershipStack(friend);
      LglsComCountHeap(friend);
      LglsComCountStack(friend);
      LglsComCountStatic(friend);
      LglsComReserveStack(friend);
      LglsComOwnershipDeepHeap(friend);
      LglsComOwnershipDeepReference(friend);
      LglsComOwnershipDeepEmergent(friend);
      LglsComHashStack(friend);
      LglsComHashHeap(friend);
      LglsComHashEmergent(friend);
      LglsComComparison(friend);
      LglsComEmplacement(friend);
      LglsComAssignment(friend);
      LglsComInsertion(friend);
      LglsComMerging(friend);
      LglsComConversion(friend);
      LglsComReserveEmergent(friend);
      LglsComIndexedHashHeap(friend);
      LglsComIndexedHashStack(friend);
      LglsComStateStack(friend);
      LglsComIndexedCommon(friend);
      LglsComIndexedCommonHashed(friend);
      LglsComIndexedLinear(friend);
      LglsComRemoval(friend);
      LglsComIterationOperators(friend);

      // Here lies the stack. It is an optimized tuple that is filled   
      // with StackRequest(s) from components.                          
      typename decltype(Inner::DefineStack(ComponentList{}))::TupleOptimized mStack;
      
      /// Default constructor doesn't initialize anything.                    
      /// Your container needs to call ConstructDefault manually.             
      constexpr Container() noexcept = default;

      /// A tag-dispatch constructor that forwards arguments to mStack.       
      /// Used in some niche container cases, like TOwn.                      
      constexpr Container(Inner::Stackwise, auto&&...arguments)
         : mStack({LglsFwd(arguments)}...) {}

      /// Default destructor does nothing. Each container has to implement    
      /// it, most likely by calling this->Destroy(). This is needed, because 
      /// the destructor relies on properly deducing 'this'.                  
      constexpr ~Container() noexcept = default;
      
      /// Get the number of heap requests in the footer for chosen heap ID    
      template<Cid SID>
      static consteval size_t CountHeapFooterRequests() {
         size_t count = 0;
         ComponentList::ForEach([&count]<class C> {
            if constexpr (requires { typename C::HeapRequest; }) {
               if constexpr (C::Id::template Contains<SID>
               and IsFooterRequest<typename C::HeapRequest>)
                  ++count;
            }
         });
         return count;
      }

      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the index in the stack tuple.   
      template<class PICK>
      static consteval size_t GetStackOffset() {
         static_assert(requires { typename PICK::StackRequest; },
            "Component data is not on the stack");
          
         size_t offset = 0;
         ComponentList::ForEachConstOr([&offset]<class C> {
            if constexpr (CT::DerivedFrom<C, PICK>)
               return true;
            else if constexpr (requires { typename C::StackRequest; }) {
               if constexpr (CT::NotVoid<typename C::StackRequest>)
                  ++offset;
               return No {};
            }
            else return No {};
         });
         return offset;
      }
      
      /// Get a reference to a heap/stack provider's data                     
      ///   @tparam ID provider ID                                            
      ///   @return return a reference to the provider's data                 
      template<Cid SID>
      static consteval auto FindProvider() {
         return ComponentList::ForEachConstOr([]<class C> {
            if constexpr (requires { C::StackProvider; }) {
               if constexpr (C::StackProvider == SID)
                  return Types<C> {};
               else
                  return No {};
            }
            else if constexpr (requires { typename C::HeapProvider; }) {
               if constexpr (C::HeapProvider::template Contains<SID>)
                  return Types<C> {};
               else
                  return No {};
            }
            else return No {};
         });
      }

      /// Go through all relevant components for the dimension 'SID' and      
      /// accumulate their header heap requests into a byte amount.           
      ///   @return The size of the heap header for the dimension, in bytes.  
      ///      The size includes alignment to the first relevant contained    
      ///      type. The header bytes are located relative to:                
      ///         GetAllocation<SID>()->GetBlockStart()                       
      template<Cid SID>
      constexpr size_t DefineHeapHeader(this auto const& self) assumptious {
         using PROVIDER = typename decltype(FindProvider<SID>())::First;
         size_t bytesize = 0;
         ComponentList::ForEach([&bytesize]<class C> {
            if constexpr (requires { typename C::HeapRequest; }) {
               if constexpr (C::Id::template Contains<SID>) {
                  using R = typename C::HeapRequest;
                  if constexpr (IsRequestModifier<R>) {
                     if constexpr (R::AllocatedPerDimension and not IsFooterRequest<R>) {
                        // Multiply only by the number of dimensions    
                        // that are shared with the relevant provider.  
                        using INTERSECT = typename C::Id::template Intersect<typename PROVIDER::Id>;
                        bytesize += sizeof(TypeOf<R>) * INTERSECT::Count;
                     }
                  }
                  else bytesize += sizeof(R);
               }
            }
         });

         const auto alignment = self.template GetAlignment<PROVIDER::Id::First>();
         return Align(bytesize, alignment);
      }      
      
      /// Go through all components until PICK is reached, and accumulate     
      /// the offset up to that point, to get the byte offset in the header   
      /// for the particular dimension 'SID'.                                 
      ///   @return The header offset, where PICK's data resides. The offset  
      ///      is relative to GetAllocation<SID>()->GetBlockStart()           
      template<class PICK, Cid SID>
      static consteval size_t GetHeapHeaderOffset() {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
         static_assert(PICK::Id::template Contains<SID>,
            "The PICK must share the provided ID");

         using PICK_R   = typename PICK::HeapRequest;
         if constexpr (IsRequestModifier<PICK_R>) {
            static_assert(not IsFooterRequest<PICK_R>,
               "Not a header request, use GetHeapFooterOffset instead"
            );
         }
         
         size_t offset = 0;
         ComponentList::ForEachConstOr([&offset]<class C> {
            if constexpr (CT::DerivedFrom<C, PICK>) {
               // Target component reached, but there might be          
               // dimensional offset to consider.                       
               using R = typename C::HeapRequest;
               if constexpr (IsRequestModifier<R>) {
                  if constexpr(R::AllocatedPerDimension) {
                     using PROVIDER = typename decltype(FindProvider<SID>())::First;
                     using INTERSECT = C::Id::template Intersect<typename PROVIDER::Id>;
                     INTERSECT::ForEachConstOr([&offset]<Cid D> {
                        if constexpr (D == SID)
                           return true;
                        else {
                           offset += sizeof(TypeOf<R>);
                           return No {};
                        }
                     });
                  }
               }
               return true;
            }
            else if constexpr (requires { typename C::HeapRequest; }) {
               if constexpr (C::Id::template Contains<SID>) {
                  using R = typename C::HeapRequest;
                  if constexpr (IsRequestModifier<R>) {
                     if constexpr (R::AllocatedPerDimension and not IsFooterRequest<R>) {
                        // Multiply only by the number of dimensions    
                        // that are shared with the relevant provider.  
                        using PROVIDER = typename decltype(FindProvider<SID>())::First;
                        using INTERSECT = C::Id::template Intersect<typename PROVIDER::Id>;
                        offset += sizeof(TypeOf<R>) * INTERSECT::Count;
                     }
                  }
                  else offset += sizeof(R);
               }
               return No {};
            }
            else return No {};
         });
         return offset;
      }

      /// Go through all components relevant to the provided dimension SID    
      /// that have PerDimension modifier, and accumulate their heap          
      /// requests into a byte amount.                                        
      ///   @param count Footer can depend on a new reserved amount           
      ///   @return The size of the heap footer for chosen dimension in bytes.
      ///      The footer starts at GetRawReserveEnd<SID>().                  
      template<Cid SID>
      constexpr size_t DefineHeapFooter(
         this auto const& self, [[maybe_unused]] const size_t count
      ) noexcept {
         size_t bytesize = 0;
         const size_t indirects = self.template GetIndirections<SID>();
         ComponentList::ForEach([&bytesize, &indirects, &count]<class C> {
            if constexpr (requires { typename C::HeapRequest; }) {
               using R = typename C::HeapRequest;
               if constexpr (IsFooterRequest<R> and C::Id::template Contains<SID>) {
                  if constexpr (R::AllocatedPerDimension) {
                     bytesize += sizeof(TypeOf<R>)
                        * (R::AllocatedPerElement     ? count      : 1)
                        * (R::AllocatedPerIndirection ? indirects  : 1);
                  }
               }
            }
         });
         return bytesize;
      }

      /// Go through all components with PerDimension modifier until PICK is  
      /// reached, and accumulate the offset up to that point, to get the byte
      /// offset in the heap for the particular dimension 'SID'.              
      ///   @param count Heap requests can depend on the amount of elements.  
      ///   @return The heap byte offset, where PICK's data resides. Relative 
      ///      to GetRawReserveEnd<SID>().                                    
      template<class PICK, Cid SID>
      constexpr size_t GetHeapFooterOffset(
         this auto const& self, [[maybe_unused]] const size_t count
      ) noexcept {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
         static_assert(PICK::Id::template Contains<SID>,
            "The PICK must share the provided ID");

         using PICK_R = typename PICK::HeapRequest;
         static_assert(IsFooterRequest<PICK_R>,
            "Not a footer request, use GetHeapHeaderOffset instead");
         static_assert(PICK_R::AllocatedPerDimension,
            "Not a PerDimension request, use GetHeapFooterOffsetGlobal instead");

         size_t offset = 0;
         const size_t indirects = self.template GetIndirections<SID>();
         ComponentList::ForEachConstOr([&offset, &indirects, &count]<class C> {
            if constexpr (CT::DerivedFrom<C, PICK>)
               return true;
            else if constexpr (requires { typename C::HeapRequest; }) {
               using R = typename C::HeapRequest;
               if constexpr (IsFooterRequest<R> and C::Id::template Contains<SID>) {
                  if constexpr (R::AllocatedPerDimension) {
                     offset += sizeof(TypeOf<R>)
                        * (R::AllocatedPerElement     ? count     : 1)
                        * (R::AllocatedPerIndirection ? indirects : 1);
                  }
               }
               return No {};
            }
            else return No {};
         });
         return offset;
      }

      /// Go through all components relevant to the provider associated with  
      /// dimension SID that have no PerDimension modifier, and accumulate    
      /// their heap requests into a byte amount.                             
      ///   @param count Footer can depend on a new reserved amount.          
      ///   @return The size of the global heap footer for the provider       
      ///      associated with the dimension 'SID'. Given 'D' is the last     
      ///      dimension for that provider, the offset is relative to:        
      ///      GetRawReserveEnd<D>() + DefineHeapFooter<D>                    
      template<Cid SID>
      constexpr size_t DefineHeapFooterGlobal(
         this auto const& self, [[maybe_unused]] const size_t count
      ) noexcept {
         size_t bytesize = 0;
         const size_t indirects = self.template GetIndirections<SID>();
         ComponentList::ForEach([&bytesize, &indirects, &count]<class C> {
            if constexpr (requires { typename C::HeapRequest; }) {
               using R = typename C::HeapRequest;
               if constexpr (IsFooterRequest<R>) {
                  if constexpr (not R::AllocatedPerDimension) {
                     using PROVIDER  = typename decltype(FindProvider<SID>())::First;
                     using INTERSECT = C::Id::template Intersect<typename PROVIDER::Id>;
                     if constexpr (not INTERSECT::Empty) {
                        bytesize += sizeof(TypeOf<R>)
                           * (R::AllocatedPerElement     ? count     : 1)
                           * (R::AllocatedPerIndirection ? indirects : 1);
                     }
                  }
               }
            }
         });
         return bytesize;
      }

      /// Go through all components relevant to the provider associated with  
      /// dimension SID that have no PerDimension modifier, and accumulate    
      ///   @param count Footer can depend on a new reserved amount.          
      ///   @return The size of the global heap footer for the provider       
      ///      associated with the dimension 'SID'. Given 'D' is the last     
      ///      dimension for that provider, the offset is relative to:        
      ///      GetRawReserveEnd<D>() + DefineHeapFooter<D>                    
      template<class PICK, Cid SID>
      constexpr size_t GetHeapFooterOffsetGlobal(
         this auto const& self, [[maybe_unused]] const size_t count
      ) noexcept {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");

         using PICK_R = typename PICK::HeapRequest;
         static_assert(IsFooterRequest<PICK_R>,
            "Not a footer request, use GetHeapHeaderOffset instead");
         static_assert(not PICK_R::AllocatedPerDimension,
            "PerDimension request, use GetHeapFooterOffset instead");

         size_t offset = 0;
         const size_t indirects = self.template GetIndirections<SID>();
         ComponentList::ForEachConstOr([&offset, &indirects, &count]<class C> {
            if constexpr (CT::DerivedFrom<C, PICK>)
               return true;
            else if constexpr (requires { typename C::HeapRequest; }) {
               using R = typename C::HeapRequest;
               if constexpr (IsFooterRequest<R>) {
                  if constexpr (not R::AllocatedPerDimension) {
                     using PROVIDER  = typename decltype(FindProvider<SID>())::First;
                     using INTERSECT = C::Id::template Intersect<typename PROVIDER::Id>;
                     if constexpr (not INTERSECT::Empty) {
                        offset += sizeof(TypeOf<R>)
                           * (R::AllocatedPerElement     ? count     : 1)
                           * (R::AllocatedPerIndirection ? indirects : 1);
                     }
                  }
               }
               return No {};
            }
            else return No {};
         });
         return offset;
      }

      /// Access a variable on the stack associated with a component          
      ///   @attention always returns a reference to valid memory             
      template<class PICK, class SELF>
      constexpr auto& AccessStack(this SELF&& self) noexcept {
         constexpr size_t IDX = GetStackOffset<PICK>();
         auto& result = ::Langulus::get<IDX>(self.mStack).value;
         using RC = LglsMutIf(SELF, decltype(result));
         return const_cast<RC>(result);
      }

      /// Access a variable on the heap of a specified dimension, associated  
      /// with a component.                                                   
      ///   @attention always returns a pointer which may be null if container
      ///      is disowned, or hasn't been heap-allocated yet. This is the    
      ///      price you pay for keeping stuff on the heap - repeated safety  
      ///      checks. Another drawback is when cached variables, like hashes,
      ///      are cached only when containers aren't disowned, thus fallback 
      ///      to emergent behavior may occur, like hashes being recomputed   
      ///      on every demand.                                               
      ///   @attention if ownership is provided, this call assumes that type  
      ///      information and reserved count have been initialized for the   
      ///      relevant dimension 'SID'                                       
      template<CT::Component PICK, Cid SID, CT::Container SELF>
      constexpr auto* AccessHeap(this SELF&& self) assumptious {
         static_assert(requires { typename PICK::HeapRequest; },
            "Component data is not on the heap");
         static_assert(PICK::Id::template Contains<SID>,
            "The PICK must share the provided ID");

         LglsAssumeDev(self.template GetAllocationInner<SID>(),
            "Heap requests are available only when container has ownership. "
            "Make sure you access them only then, and fallback to emergent "
            "behavior otherwise, if that's possible."
         );

         using R = typename PICK::HeapRequest;
         if constexpr (IsFooterRequest<R>) {
            // Access footer heap                                       
            if constexpr (R::AllocatedPerDimension) {
               // Positioned after each dimension data                  
               const size_t reserved = self.template GetReserved<SID>();
               const size_t stride = self.template GetStride<SID>();
               const size_t offset = self.template GetHeapFooterOffset<PICK, SID>(reserved);
               const auto heap = self.template GetRawAs<uint8_t, SID>();
               using RC = LglsMutIf(SELF, TypeOf<R>*);
               return reinterpret_cast<RC>(heap + reserved * stride + offset);
            }
            else {
               // Global footer, positioned after the last dimension    
               using PROVIDER = typename decltype(FindProvider<SID>())::First;
               constexpr Cid LAST_ID = PROVIDER::Id::Last;
               const size_t reserved = self.template GetReserved<LAST_ID>();
               const size_t stride = self.template GetStride<LAST_ID>();
               const size_t offset_local = self.template DefineHeapFooter<LAST_ID>(reserved);
               const size_t offset_global = self.template GetHeapFooterOffsetGlobal<PICK, SID>(reserved);
               const auto heap = self.template GetRawAs<uint8_t, LAST_ID>();
               using RC = LglsMutIf(SELF, TypeOf<R>*);
               return reinterpret_cast<RC>(heap + reserved * stride + offset_local + offset_global);
            }
         }
         else {
            // Access header heap                                       
            constexpr size_t offset = GetHeapHeaderOffset<PICK, SID>();
            const auto al   = self.template GetAllocationInner<SID>();
            const auto heap = al->GetBlockStart();
            using RC = LglsMutIf(SELF, R*);
            return reinterpret_cast<RC>(heap + offset);
         }
      }
      
      /// Checks whether at least one of the components has a method with the 
      /// given name and signature. Undefined at the end of this container.   
      /*#define if_inherits(...) requires ( \
         (requires (COMPONENTS t) { {t.__VA_ARGS__}; }) or ... \
      )*/

      /// Propagates method, by calling it in all components where it exists. 
      /// Entirely disables the method for the container, if not found.       
      /// Macro is #undeffed at the end of this container to avoid pollution. 
      /*#define unify_compose(name) \
         template<class SELF> \
         constexpr void name(this SELF&& self) noexcept if_inherits(name()) { \
            ComponentList::ForEach([&]<class C>{ \
               if_available(self.C::name()); \
            }); \
         }*/

      /// Propagates method, by calling it in all components where it exists, 
      /// as long as the ID is satisfied.                                     
      /// Entirely disables the method for the container, if not found.       
      /// Macro is #undeffed at the end of this container to avoid pollution. 
      /*#define unify_compose_relevant(name) \
         template<Cid SID = 0, class SELF> \
         constexpr void name(this SELF&& self) noexcept if_inherits(template name<SID>()) { \
            ComponentList::ForEach([&]<class C>{ \
               if_available(self.C::template name<SID>()); \
            }); \
         }*/

      template<class SELF>
      constexpr void ConstructDefault(this SELF&& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::ConstructDefault());
         });
      }

      template<Cid SID = 0, class SELF>
      constexpr void ConstructHeapRequest(this SELF&& self) noexcept {
         ComponentList::ForEach([&]<class C>{
            if_available(self.C::template ConstructHeapRequest<SID>());
         });
      }

      //unify_compose(ConstructDefault);
      //unify_compose_relevant(ConstructHeapRequest);

      /// Call ConstructFrom in all components that implement it.             
      /// Fallback to ConstructDefault otherwise.                             
      template<CT::Container SELF, CT::Container FROM>
      constexpr void Absorb(this SELF& self, FROM&& from) {
         static_assert(CT::Contiguous<SELF> == CT::Contiguous<FROM>,
            "You can't absorb from containers with different contiguousness");

         ComponentList::ForEach([&]<class C>{
                 if_available(self.C::ConstructFrom(FWDIntent(from)))
            else if_available(self.C::ConstructDefault())
         });
      }

      /// Call Destroy in all components that implement it.                   
      /// Always do it in reverse order!                                      
      constexpr void Destroy(this auto& self) {
         if not consteval {
            ComponentList::Reverse::ForEach([&]<class C> {
               if_available(self.C::Destroy());
            });
         }
      }

      /*template<bool FIND_MISSING = true, Cid ID = 0, class SELF>
      void KeepElementDeep(this SELF&& self) noexcept
      if_inherits(template KeepElementDeep<FIND_MISSING, ID>()) {
         ComponentList::ForEachConstOr([&]<class C> {
            if constexpr (requires { self.C::template KeepElementDeep<FIND_MISSING, ID>(); }) {
               self.C::template KeepElementDeep<FIND_MISSING, ID>();
               return true;
            }
            else return No {};
         });
      }

      template<bool FORCE_DESTROY = true, Cid ID = 0, class SELF>
      void DestroyElementDeep(this SELF&& self) noexcept
      if_inherits(template DestroyElementDeep<FORCE_DESTROY, ID>()) {
         ComponentList::Reverse::ForEachConstOr([&]<class C> {
            if constexpr (requires { self.C::template DestroyElementDeep<FORCE_DESTROY, ID>(); }) {
               self.C::template DestroyElementDeep<FORCE_DESTROY, ID>();
               return true;
            }
            else return No {};
         });
      }
      
      template<bool FORCE_DESTROY = true, Cid ID = 0, class SELF>
      void DestroyElement(this SELF& self) assumptious
      if_inherits(template DestroyElement<FORCE_DESTROY, ID>()) {
         ComponentList::Reverse::ForEachConstOr([&]<class C> {
            if constexpr (requires { self.C::template DestroyElement<FORCE_DESTROY, ID>(); }) {
               self.C::template DestroyElement<FORCE_DESTROY, ID>();
               return true;
            }
            else return No {};
         });
      }
      
      template<bool FORCE_DESTROY = true, Cid ID = 0, class SELF>
      void DestroyAllElements(this SELF& self) assumptious
      if_inherits(template DestroyAllElements<FORCE_DESTROY, ID>()) {
         ComponentList::Reverse::ForEachConstOr([&]<class C> {
            if constexpr (requires { self.C::template DestroyAllElements<FORCE_DESTROY, ID>(); }) {
               self.C::template DestroyAllElements<FORCE_DESTROY, ID>();
               return true;
            }
            else return No {};
         });
      }

      template<Cid ID = 0, class SELF>
      void DestroyElementShallow(this SELF& self) noexcept 
      if_inherits(template DestroyElementShallow<ID>()) {
         ComponentList::Reverse::ForEachConstOr([&]<class C> {
            if constexpr (requires { self.C::template DestroyElementShallow<ID>(); }) {
               self.C::template DestroyElementShallow<ID>();
               return true;
            }
            else return No {};
         });
      }*/

      /// Get a handle to the first element(s). Very useful for internal use. 
      /// No-op if C is already a handle, even if AS is specified.            
      ///   @attention element might be uninitialized if C is discontiguous   
      ///   @tparam AS the handle type, or void to decide automatically       
      ///   @tparam SID the shared heap entry ID                              
      ///   @return the handle to the first element. This element might not   
      ///      be initialized if C is discontiguous!                          
      template<class AS = void, Cid SID = 0, CT::NotHandle C>
      decltype(auto) GetHandle(this C&& self) {
         static_assert(CT::Handle<AS> or CT::Void<AS>,
            "Must be either a handle or void (which will use DecideHandle");
         static_assert(not CT::Reference<AS>,
            "Strip references first");
         static_assert(CT::Dense<AS>,
            "Must be dense");

         using H = Tif<CT::Void<AS>, DecideHandle<C>, AS>;
         if constexpr (CT::Pair<H>) {
            // User desires a pair, so we give them a pair              
            using H1 = typename H::KeyHandle;
            using H2 = typename H::ValHandle;
            return H {
               self.template GetHandle<H1, SID + 0>(),
               self.template GetHandle<H2, SID + 1>()
            };
         }
         else {
            // User desires a simple handle                             
            if constexpr (CT::TypeErased<H>) {
               // Type-erased handle                                    
               if constexpr (CT::DeeplyOwned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetEntries<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else if constexpr (CT::Owned<H>) {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetAllocation<SID>(),
                     self.template GetType<SID>()
                  };
               }
               else {
                  return H {
                     self.template Get<void, SID>(),
                     self.template GetType<SID>()
                  };
               }
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<H>>;

               if constexpr (CT::TypeErased<C>) {
                  LglsAssert(self.template GetType<SID>().IsSame(MetaDataOf<HT>()),
                     "Type mismatch", ": ", self.template GetType<SID>(),
                     " not same as ", MetaDataOf<HT>()
                  );
               }
               else static_assert(Same<TypeOf<C, SID>, HT>, "Type mismatch");

               if constexpr (CT::DeeplyOwned<H>) {
                  if constexpr (requires { H::Emergent; })
                     return H {self.template Get<void, SID>()};
                  else {
                     return H {
                        self.template Get<void, SID>(),
                        self.template GetEntries<SID>()
                     };
                  }
               }
               else if constexpr (CT::Owned<H>) {
                  if constexpr (requires { H::Emergent; })
                     return H {self.template Get<void, SID>()};
                  else {
                     return H {
                        self.template Get<void, SID>(),
                        self.template GetAllocation<SID>()
                     };
                  }
               }
               else return H {&self.template Get<void, SID>()};
            }
         }
      }

      /// No-op in case C is already a handle                                 
      template<class = void, Cid SID = 0, CT::Handle C>
      constexpr C&& GetHandle(this C&& self) noexcept {
         static_assert(SID == 0);//TODO maybe not noop? what if we want to get the first subhandle from a pair-handle?
         return LglsFwd(self);
      }
      
   //public:
      /// Visit all element's handles and perform a function on them.         
      /// Handles both linear and non-linear containers gracefully.           
      ///   @tparam SKIP_EMPTY whether or not to skip empty elements inside   
      ///      maps/sets. If set to false, you have to check whether the      
      ///      current lambda argument is `if constexpr(CT::Supported)`       
      ///   @param lambda the function to perform. If the lambda returns bool,
      ///      you can end the loop early by returning false.                 
      ///   @param cookie the element/hash table spot to start off from       
      ///   @attention this will ignore any ordering                          
      ///   @attention assumes container isn't empty                          
      template<bool SKIP_EMPTY = true, CT::Container C>
      void Apply(this C& self, auto&& lambda, [[maybe_unused]] size_t cookie = 0) {
         LglsAssumeDev(not self.IsEmpty(), "Make sure container isn't empty");

         if constexpr (CT::ContainsOne<C>) {
            //TODO GetHandle here is redundant, but most use cases      
            // of Apply require it.                                     
            lambda(self.GetHandle());
         }
         else {
            auto item = self.GetHandle() + cookie;

            if constexpr (CT::Contiguous<C>) {
               // Iterate a contiguous array of elements                
               LglsAssumeDev(cookie < self.GetCount(), "Limp cookie (contiguous)");
               auto const end = item + (self.GetCount() - cookie);
               while (item.GetRaw() != end.GetRaw()) {
                  if constexpr (CT::Bool<decltype(lambda(item))>) {
                     if (not lambda(item))
                        return;
                  }
                  else lambda(item);
                  ++item;
               }
            }
            else {
               // Iterate a hash table - some cells might be empty,     
               // thus container might not be a contiguous array        
               LglsAssumeDev(cookie < self.GetReserved(), "Limp cookie (discontiguous)");
               const auto tableBeg = self.GetHashTableInner() + cookie;
               const auto tableEnd = tableBeg + (self.GetReserved() - cookie);
               auto table = tableBeg;
               while (table != tableEnd) {
                  if (*table) {
                     if constexpr (CT::Bool<decltype(lambda(item))>) {
                        if (not lambda(item))
                           return;
                     }
                     else lambda(item);
                  }
                  else if constexpr (not SKIP_EMPTY) {
                     if constexpr (CT::Bool<decltype(lambda(Unsupported{}))>) {
                        if (not lambda(Unsupported{}))
                           return;
                     }
                     else lambda(Unsupported{});
                  }

                  ++item;
                  ++table;
               }
            }
         }
      }

   public:
      ///                                                                     
      /// THE UNIFYING INTERFACE                                              
      ///                                                                     
      /// All the following methods exist only if the appropriate method      
      /// equivalents exist in one or more components.                        
      
      /// Call AssignFrom in all components that implement it.                
      /// Fallback to AssignDefault otherwise.                                
      template<CT::Container SELF, CT::Container FROM>
      constexpr SELF& AssignAbsorb(this SELF& self, FROM&& rhs) {
         static_assert(CT::Contiguous<SELF> == CT::Contiguous<FROM>,
            "You can't assign-absorb from containers with different contiguousness");

         ComponentList::ForEach([&]<class C>{
            if_available(self.C::AssignFrom(FWDIntent(rhs)))
            //else if_available(self.C::AssignDefault())
         });
         return self;
      }
   
      /*#define unify_getter(name) \
         template<Cid ID = 0, class SELF> \
         constexpr decltype(auto) name(this SELF&& self) \
         if_inherits(template name<ID>()) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ID>(); }) \
                  return self.C::template name<ID>(); \
               else return No{}; \
            }); \
         }

      #define unify_getter_argumented(name) \
         template<Cid ID = 0, class SELF> \
         constexpr decltype(auto) name(this SELF&& self, auto&&...arguments) \
         if_inherits(template name<ID>(LglsFwd(arguments)...)) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ID>(LglsFwd(arguments)...); }) \
                  return self.C::template name<ID>(LglsFwd(arguments)...); \
               else return No{}; \
            }); \
         }

      #define unify_getter_templated(name) \
         template<class ARG, Cid ID = 0, class SELF> \
         constexpr decltype(auto) name(this SELF&& self) \
         if_inherits(template name<ARG, ID>()) { \
            return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) { \
               if constexpr (requires { self.C::template name<ARG, ID>(); }) \
                  return self.C::template name<ARG, ID>(); \
               else return No{}; \
            }); \
         }

      #define unify_setter(name) \
         template<Cid ID = 0, class SELF> \
         constexpr decltype(auto) name(this SELF& self, auto&&...arguments) \
         if_inherits(template name<ID>(LglsFwd(arguments)...)) { \
            ComponentList::ForEachConstOr([&]<class C> { \
               if constexpr (requires { self.C::template name<ID>(LglsFwd(arguments)...); }) { \
                  self.C::template name<ID>(LglsFwd(arguments)...); return 1; \
               } else return No{}; \
            }); \
         }

      #define unify_setter_templated(name) \
         template<class ARG, Cid ID = 0, class SELF> \
         constexpr decltype(auto) name(this SELF& self) \
         if_inherits(template name<ARG, ID>()) { \
            ComponentList::ForEachConstOr([&]<class C> { \
               if constexpr (requires { self.C::template name<ARG, ID>(); }) { \
                  self.C::template name<ARG, ID>(); return 1; \
               } else return No{}; \
            }); \
         }

      unify_getter(GetRaw);
      unify_getter(GetType);
      unify_getter(IsTyped);
      unify_getter(IsSparse);
      unify_getter(IsDeep);
      unify_getter(IsConstant);
      unify_getter(IsExecutable);
      unify_getter(IsTypeConstrained);*/

      /*template<Cid ID = 0>
      constexpr bool IsExecutable(this auto const& self) noexcept
      if_inherits(template IsExecutable<ID>()) {
         return ComponentList::ForEachConstOr([&]<class C> -> decltype(auto) {
            if constexpr (requires { self.C::template IsExecutable<ID>(); })
               return self.C::template IsExecutable<ID>();
            else return No{};
         });
      }*/

      /*unify_getter(GetCount);
      unify_getter(GetReserved);
      unify_getter(GetIndirections);
      unify_getter(GetStride);
      unify_getter(GetBytesize);
      unify_getter(GetAlignment);
      unify_getter(GetEntries);
      unify_getter(GetAllocation);
      unify_getter(GetUses);
      unify_getter_argumented(Is);
      unify_getter_argumented(IsSame);
      unify_getter_argumented(IsExact);
      unify_getter_argumented(GetEntriesAt);
      unify_getter_templated(As);
      unify_getter_templated(GetRawAs);
      unify_getter_templated(Is);
      unify_getter_templated(IsSame);
      unify_getter_templated(IsExact);

      template<class AS = void, Cid ID = 0, class SELF>
      constexpr auto* Get(this SELF&& self) assumptious
      if_inherits(template Get<AS, ID>()) {
         return ComponentList::ForEachConstOr([&]<class C> assumptious {
            if constexpr (requires { self.C::template Get<AS, ID>(); })
               return self.C::template Get<AS, ID>();
            else return No{};
         });
      }

      template<Cid ID = 0, class AS = void, class SELF>
      constexpr auto GetDense(this SELF&& self, size_t count = -1) assumptious
      if_inherits(template GetDense<ID, AS>(count)) {
         return ComponentList::ForEachConstOr([&]<class C> assumptious {
            if constexpr (requires { self.C::template GetDense<ID, AS>(count); })
               return self.C::template GetDense<ID, AS>(count);
            else return No{};
         });
      }

      unify_setter(SetType);
      unify_setter_templated(SetType);*/

   protected:
      /*unify_getter(GetHeapInner);
      unify_getter(GetAllocationInner);
      unify_getter(GetEntriesInner);
      unify_getter(GetRawVoid);
      unify_setter(SetReservedInner);
      unify_setter(SetHeapInner);
      unify_setter(SetAllocationInner);
      unify_setter(EmplaceEntries);
      unify_setter(AllocateFresh);
      unify_setter(AllocateLess);
      unify_setter(AllocateMore);
      unify_getter_argumented(RequestHeap);

      #undef if_inherits
      #undef unify_compose
      #undef unify_compose_relevant
      #undef unify_getter
      #undef unify_getter_templated
      #undef unify_getter_argumented
      #undef unify_setter
      #undef unify_setter_templated*/
   };
   }
}

namespace Langulus
{
   /// Loop controls from inside ForEach lambdas when iterating containers    
   struct LoopControl {
      enum Command : int {
         Break = 0,     // Break the loop                               
         Continue = 1,  // Continue the loop                            
         Repeat = 2,    // Repeat the current element                   
         Discard = 3,   // Remove the current element                   
         NextLoop = 4   // Skip to next function in the ForEach         
      } mControl;

      LoopControl() = delete;

      constexpr LoopControl(bool a) noexcept
         : mControl {static_cast<Command>(a)} {}
      constexpr LoopControl(Command a) noexcept
         : mControl {a} {}

      explicit constexpr operator bool() const noexcept {
         return mControl == Continue or mControl == Repeat;
      }

      constexpr bool operator == (const LoopControl&) const noexcept = default;
   };

   namespace Loop
   {
      /// Break the entire iteration as a whole                               
      constexpr LoopControl Break    = LoopControl::Break;
      /// Continue to next element or function                                
      constexpr LoopControl Continue = LoopControl::Continue;
      /// Repeat the current element                                          
      constexpr LoopControl Repeat   = LoopControl::Repeat;
      /// Remove the current element                                          
      constexpr LoopControl Discard  = LoopControl::Discard;
      /// End this iterating function and jump immediately to the next        
      constexpr LoopControl NextLoop = LoopControl::NextLoop;
   }
}
