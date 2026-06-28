///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Langulus/CT/Deep.hpp"
#include "Langulus/Core.hpp"
#include "source/Component.hpp"
#include "source/Container.hpp"
#include <source/components/Typed-Stack.hpp>
#include <source/components/Typed-Static.hpp>
#include <source/components/Heap-Reference.hpp>
#include <source/components/Count-Static.hpp>
#include <source/components/Reserve-Emergent.hpp>
#include <source/components/OwnershipDeep-Heap.hpp>
#include <source/components/OwnershipDeep-Reference.hpp>
#include <source/components/Hash-Emergent.hpp>
#include <source/components/Assignment.hpp>
#include <source/components/Emplacement.hpp>
#include <source/components/Comparison.hpp>
#include <source/components/Iteration-Operators.hpp>
#include <source/components/Stack.hpp>


namespace Langulus::Anyness
{
   namespace Inner
   {
      using TypeErasedHandleMut = Com::Container<
         Com::TypedStack<DMeta, void, true>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepReference<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;

      using TypeErasedHandleMutDisowned = Com::Container<
         Com::TypedStack<DMeta, void, true>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;

      using TypeErasedHandle = Com::Container<
         Com::TypedStack<DMeta, void, true>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepReference<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;

      using TypeErasedHandleDisowned = Com::Container<
         Com::TypedStack<DMeta, void, true>,
         Com::HeapReference<>,
         Com::CountStatic<1u>,
         Com::HashEmergent<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;

      /// Statically typed handle to a dense element held inside a container  
      template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleEmbeddedDense = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<HeapEntry<0, Deref<T>*>>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::OwnershipStack<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a sparse element held inside a container 
      template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleEmbeddedSparse = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<HeapEntry<0, Deref<T>*>>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepReference<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a dense element held inside a container  
      template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleEmbeddedDenseEmergent = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<HeapEntry<0, Deref<T>*>>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::OwnershipEmergent<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;

      /// Statically typed handle to a sparse element held inside a container 
      /// (with emergent deep ownership)                                      
      template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleEmbeddedSparseEmergent = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<HeapEntry<0, Deref<T>*>>,
         Com::CountStatic<1u>,
         Com::OwnershipDeepEmergent<Com::WeakOwnership>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a disowned element held inside container 
      template<CT::Reference T> requires (CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleDisownedEmbedded = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::HeapReference<HeapEntry<0, Deref<T>*>>,
         Com::CountStatic<1u>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>,
         Com::IterationOperators<>
      >;
      
      /// Statically typed handle to a local dense value                      
      /// (isomorphic to TOwn)                                                
      //TODO inherit TOwn from this?
      template</*CT::NotReference*/class T> requires (CT::Dense<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleLocalDense = Com::Container<
         Com::TypedStatic<DMeta, Deref<T>>,
         Com::Stack<T>,
         Com::CountStatic<1u>,
         Com::HashEmergent<>,
         Com::Assignment<>,
         Com::Emplacement<>,
         Com::Comparison<>
      >;
      
      /// Statically typed handle to a local sparse value.                    
      ///   @attention this handle is local and has strong ownership!         
      template<CT::NotReference T> requires (CT::Sparse<T> and CT::NotSheddable<T> and CT::NotHandle<T>)
      using THandleLocalSparse = Com::Container<
         Com::TypedStatic<DMeta, T>,
         Com::HeapMovable<0, 0, HeapEntry<0, T*>>,
         Com::CountStatic<1u>,
         Com::ReserveEmergent<>,
         Com::OwnershipStack<>,
         Com::OwnershipDeepHeap<>,
         Com::HashEmergent<>,
         Com::Emplacement<>,
         Com::Assignment<>,
         Com::Comparison<>
      >;
   }


   /// MARK: HandleMut                                                        
   ///                                                                        
   /// A type-erased mutable handle with ownership.                           
   /// It refers to a picked element inside a type-erased container.          
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment                                
   struct HandleMut : Inner::TypeErasedHandleMut {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using DeepType       = HandleDisowned; //TODO why disowned??

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //HandleMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleMut() noexcept {
         this->ConstructDefault();
      }

      constexpr HandleMut(HandleMut const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr HandleMut(HandleMut&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr HandleMut(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr HandleMut(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr HandleMut(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::TypeErasedHandleMut {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      HandleMut& operator = (HandleMut const& other) = delete;
      HandleMut& operator = (HandleMut&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> HandleMut& {
         return *this;
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   /// MARK: HandleDisownedMut                                                
   ///                                                                        
   /// A type-erased mutable handle without ownership.                        
   /// It refers to a picked element inside a type-erased container.          
   struct HandleDisownedMut : Inner::TypeErasedHandleMutDisowned {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using DeepType       = HandleDisowned; //TODO why disowned??

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //HandleDisownedMut(Inner::Piecewise, auto&&) = delete;

      constexpr HandleDisownedMut() noexcept {
         this->ConstructDefault();
      }

      constexpr HandleDisownedMut(HandleDisownedMut const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr HandleDisownedMut(HandleDisownedMut&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr HandleDisownedMut(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr HandleDisownedMut(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr HandleDisownedMut(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::TypeErasedHandleMutDisowned {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      HandleDisownedMut& operator = (HandleDisownedMut const& other) = delete;
      HandleDisownedMut& operator = (HandleDisownedMut&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> HandleDisownedMut& {
         return *this;
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   /// MARK: Handle                                                           
   ///                                                                        
   /// A type-erased immutable handle with ownership.                         
   /// It refers to a picked element inside a type-erased container.          
   ///   @attention handles are never (de)referenced upon construction and    
   ///      destruction - only on reassignment. Since this handle is not      
   ///      mutable, this isn't possible either, however the handle still     
   ///      carries ownership information, so that it can be used on demand   
   ///      instead of sought from the memory manager every time.             
   struct Handle : Inner::TypeErasedHandle {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using DeepType       = HandleDisowned; //TODO why disowned???

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //Handle(Inner::Piecewise, auto&&) = delete;

      constexpr Handle() noexcept {
         this->ConstructDefault();
      }

      constexpr Handle(Handle const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr Handle(Handle&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr Handle(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr Handle(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr Handle(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::TypeErasedHandle {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      Handle& operator = (Handle const& other) = delete;
      Handle& operator = (Handle&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> HandleMut& {
         return *reinterpret_cast<HandleMut*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   /// MARK: HandleDisowned                                                   
   ///                                                                        
   /// A type-erased immutable handle without ownership.                      
   /// It refers to a picked element inside a type-erased container.          
   struct HandleDisowned : Inner::TypeErasedHandleDisowned {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using DeepType       = HandleDisowned;

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //HandleDisowned(Inner::Piecewise, auto&&) = delete;

      constexpr HandleDisowned() noexcept {
         this->ConstructDefault();
      }

      /// Refer constructor                                                   
      constexpr HandleDisowned(HandleDisowned const& other) noexcept {
         this->Absorb(Disown(other));
      }

      /// Move constructor                                                    
      constexpr HandleDisowned(HandleDisowned&& other) noexcept {
         this->Absorb(Disown(other));
      }

      /// Construction that absorbs the provided container                    
      constexpr HandleDisowned(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr HandleDisowned(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr HandleDisowned(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::TypeErasedHandleDisowned {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      HandleDisowned& operator = (HandleDisowned const& other) = delete;
      HandleDisowned& operator = (HandleDisowned&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> HandleDisownedMut& {
         return *reinterpret_cast<HandleDisownedMut*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };

   
   /// MARK: THandle                                                          
   ///                                                                        
   /// When T is a reference, then element is embedded inside container       
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleEmbeddedDense<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandle;
      using DeepType       = HandleDisowned; //TODO why disowned??

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

      constexpr THandle(THandle const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandle(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr THandle(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr THandle(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::THandleEmbeddedDense<T> {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      THandle& operator = (THandle const& other) = delete;
      THandle& operator = (THandle&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandle<Decvq<Deref<T>>&>& {
         return *reinterpret_cast<THandle<Decvq<Deref<T>>&>*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   
   template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleEmbeddedSparse<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned; //TODO why disowned??

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //THandle(Inner::Piecewise, auto&&) = delete;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

      constexpr THandle(THandle const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandle(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr THandle(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr THandle(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::THandleEmbeddedSparse<T> {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      THandle& operator = (THandle const& other) = delete;
      THandle& operator = (THandle&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandle<Decvq<Deref<T>>&>& {
         return *reinterpret_cast<THandle<Decvq<Deref<T>>&>*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   
   
   /// MARK: THandleEmergent                                                  
   ///                                                                        
   /// When T is a reference, then element is embedded inside container       
   ///   @attention memory is never (de)referenced upon construction and      
   ///      destruction - only on reassignment                                
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
   struct THandleEmergent<T> : Inner::THandleEmbeddedDenseEmergent<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandleEmergent;
      using DeepType       = HandleDisowned; //TODO why disowned??

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //THandleEmergent(Inner::Piecewise, auto&&) = delete;

      constexpr THandleEmergent() noexcept {
         this->ConstructDefault();
      }

      constexpr THandleEmergent(THandleEmergent const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleEmergent(THandleEmergent&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleEmergent(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr THandleEmergent(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr THandleEmergent(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::THandleEmbeddedDenseEmergent<T> {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      THandleEmergent& operator = (THandleEmergent const& other) = delete;
      THandleEmergent& operator = (THandleEmergent&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandleEmergent<Decvq<Deref<T>>&>& {
         return *reinterpret_cast<THandleEmergent<Decvq<Deref<T>>&>*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };


   template<CT::Reference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
   struct THandleEmergent<T> : Inner::THandleEmbeddedSparseEmergent<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      //using CTTI_Typed     = Deref<T>;
      using CTTI_ReflectAs = void;
      using Denser         = THandleEmergent<Deptr<T>&>;
      using DeepType       = HandleDisowned; //TODO why disowned??

      //static constexpr bool Emergent = true;

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //THandleEmergent(Inner::Piecewise, auto&&) = delete;

      constexpr THandleEmergent() noexcept {
         this->ConstructDefault();
      }

      constexpr THandleEmergent(THandleEmergent const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleEmergent(THandleEmergent&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleEmergent(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr THandleEmergent(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr THandleEmergent(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::THandleEmbeddedSparseEmergent<T> {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      THandleEmergent& operator = (THandleEmergent const& other) = delete;
      THandleEmergent& operator = (THandleEmergent&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandleEmergent<Decvq<Deref<T>>&>& {
         return *reinterpret_cast<THandleEmergent<Decvq<Deref<T>>&>*>(this);
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   /// MARK: THandleDisowned                                                  
   ///                                                                        
   /// When T is a reference, then element is embedded inside container.      
   /// This handle never propagates or modifies ownership.                    
   ///   @tparam T the contained type                                         
   template<CT::Reference T> requires CT::NotSheddable<T>
   struct THandleDisowned<T> : Inner::THandleDisownedEmbedded<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandle<Deptr<T>&>;
      using DeepType       = HandleDisowned;

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      /// Handles can't be piecewise-initialized                              
      //THandleDisowned(Inner::Piecewise, auto&&) = delete;

      constexpr THandleDisowned() noexcept {
         this->ConstructDefault();
      }

      constexpr THandleDisowned(THandleDisowned const& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleDisowned(THandleDisowned&& other) noexcept {
         this->Absorb(Disown(other));
      }

      constexpr THandleDisowned(CT::Container auto&& other) noexcept {
         this->Absorb(Disown(other));
      }

      template<Cid SID>
      constexpr THandleDisowned(Inner::Slice<SID>, CT::Container auto&& other) noexcept {
         this->template SliceFrom<SID>(Disown(other));
      }

      constexpr THandleDisowned(Inner::Stackwise, auto&&...arguments) noexcept
         : Inner::THandleDisownedEmbedded<T> {Stackwise, LglsFwd(arguments)...} {}

      /// Assignment is disabled                                              
      THandleDisowned& operator = (THandleDisowned const& other) = delete;
      THandleDisowned& operator = (THandleDisowned&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandleDisowned<Decvq<Deref<T>>&>& {
         return *this;
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   /// MARK: THandle local                                                    
   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such dense handles are similar to TOwn<T> - data is on the stack.      
   ///   @tparam T the contained type                                         
   template<CT::NotReference T> requires (CT::Dense<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleLocalDense<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandle<T&>; // avoids nested local handles (and thus copies) by adding a reference
      using DeepType       = HandleDisowned; //TODO why disowned??
      using Base           = typename Inner::THandleLocalDense<T>::Base;

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      constexpr THandle() noexcept = default;

      /// Absorb constructors                                                 
      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandle(Inner::Absorb, CT::Container auto&& other) {
         this->Absorb(LglsFwd(other));
      }

      /// Piecewise constructors                                              
      /// (for local dense handles, piecewise == stackwise)                   
      constexpr THandle(Inner::Stackwise, auto&& a)
      requires requires { T{LglsFwd(a)}; }
         : Base {Stackwise, LglsFwd(a)} {}

      constexpr THandle(Inner::Stackwise, CT::Intent auto&& a)
      requires (not requires { T{LglsFwd(a)}; })
         : Base {Stackwise, DeintCast(a)} {}

      constexpr THandle(Inner::Piecewise, auto&& a)
      requires requires { T{LglsFwd(a)}; }
         : Base {Stackwise, LglsFwd(a)} {}

      constexpr THandle(Inner::Piecewise, CT::Intent auto&& a)
      requires (not requires { T{LglsFwd(a)}; })
         : Base {Stackwise, DeintCast(a)} {}

      template<NotTag ALT_T> requires (not CT::DeepDense<ALT_T>)
      constexpr THandle(ALT_T&& a) requires requires { T{LglsFwd(a)}; }
         : Base {Stackwise, LglsFwd(a)} {}

      template<NotTag ALT_T> requires (CT::Intent<ALT_T> and not CT::DeepDense<ALT_T>)
      constexpr THandle(ALT_T&& a) requires (not requires { T{LglsFwd(a)}; })
         : Base {Stackwise, DeintCast(a)} {}

      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      /// Assignment is disabled                                              
      THandle& operator = (THandle const& other) = delete;
      THandle& operator = (THandle&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandle<Decvq<T>>& {
         return *this;
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
   

   ///                                                                        
   /// When T is not a reference, then it is not embedded.                    
   /// Such sparse handles are similar to TRef<Deptr<T>>.                     
   ///   @attention such handles are local and have strong ownership! This    
   ///      means that they need to be cleared of their allocation upon move  
   ///      or abandon!                                                       
   ///   @tparam T the contained sparse type                                  
   template<CT::NotReference T> requires (CT::Sparse<T> and CT::NotSheddable<T>)
   struct THandle<T> : Inner::THandleLocalSparse<T> {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_ReflectAs = void;
      using Denser         = THandle<Deptr<T>&>; // avoids nested local handles (and thus copies) by adding a reference
      using DeepType       = HandleDisowned; //TODO why disowned??
      using Base           = typename Inner::THandleLocalSparse<T>::Base;

      template<CT::Handle, CT::Handle> friend struct THandlePair;

      constexpr THandle() noexcept {
         this->ConstructDefault();
      }

      constexpr THandle(THandle const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandle(THandle&& other) noexcept {
         this->Absorb(Move(other));
      }

      /// Piecewise constructor                                               
      template<class A>
      THandle(Inner::Piecewise, A&& pointer) {
         if (DeintCast(pointer))
            this->EmplaceConstruct(LglsFwd(pointer));
         else
            this->ConstructDefault();
      }

      constexpr ~THandle() noexcept {
         this->Destroy();
      }

      /// Assignment is disabled                                              
      THandle& operator = (THandle const& other) = delete;
      THandle& operator = (THandle&& other) = delete;

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandle<Decvq<T>>& {
         return *this;
      }

      /// Pick a specific dimension if handle is complex (like THandlePair).  
      /// In this case it returns itself for dimension #0.                    
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
         static_assert(SID == 0, "No such dimension");
         return LglsFwd(self);
      }
   };
}

namespace Langulus::CTTI
{
   /// MARK: Converters                                                       
   /// Convert Handle -> Text                                                 
   template<>
   struct Converter<Anyness::Handle, Anyness::Text> {
      static constexpr auto Convert(Anyness::Handle const&) -> Anyness::Text;
   };

   /// Convert HandleMut -> Text                                              
   template<>
   struct Converter<Anyness::HandleMut, Anyness::Text> {
      static constexpr auto Convert(Anyness::HandleMut const&) -> Anyness::Text;
   };
   
   /// Convert HandleDisowned -> Text                                         
   template<>
   struct Converter<Anyness::HandleDisowned, Anyness::Text> {
      static constexpr auto Convert(Anyness::HandleDisowned const&) -> Anyness::Text;
   };
   
   /// Convert HandleDisownedMut -> Text                                      
   template<>
   struct Converter<Anyness::HandleDisownedMut, Anyness::Text> {
      static constexpr auto Convert(Anyness::HandleDisownedMut const&) -> Anyness::Text;
   };
   
   /// Convert THandle -> Text                                                
   template<class T>
   struct Converter<Anyness::THandle<T>, Anyness::Text> {
      static constexpr auto Convert(Anyness::THandle<T> const&) -> Anyness::Text;
   };
   
   /// Convert THandleDisowned -> Text                                        
   template<class T>
   struct Converter<Anyness::THandleDisowned<T>, Anyness::Text> {
      static constexpr auto Convert(Anyness::THandleDisowned<T> const&) -> Anyness::Text;
   };
   
   /// Convert THandleEmergent -> Text                                        
   template<class T>
   struct Converter<Anyness::THandleEmergent<T>, Anyness::Text> {
      static constexpr auto Convert(Anyness::THandleEmergent<T> const&) -> Anyness::Text;
   };
}
