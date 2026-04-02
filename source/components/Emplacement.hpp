///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "IndexedLinear.hpp"
#include <Langulus/CT/Describable.hpp>
#include <Langulus/Allocator.hpp>
#include <Langulus/MetaOf.hpp>


namespace Langulus::CT
{
   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible.        
   ///   @attention type-erased elements are always emplaceable, because      
   ///      all arguments will be encapsulated in a descriptor, and will fail 
   ///      at runtime if not reflected as descriptor-constructible           
   template<class C, class...A>
   concept RangeEmplaceable = Container<C> and (
      Untyped<C> or ::std::constructible_from<TypeOf<C>, A...>
   );
}

namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements emplacement for containers.                                 
   /// Unlike insertion, emplacement reuses the same memory space and         
   /// guarantees that nothing moves around.                                  
   ///   @tparam ID heap we're emplacing to                                   
   template<Cid ID>
   struct Emplacement {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   protected:
      template<Cid, uint, uint, CT::HeapEntry...> friend struct HeapMovable;
      template<Cid, class>          friend struct Insertion;
      template<Cid, class>          friend struct Merging;
      template<Cid, class, Cid...>  friend struct IndexedCommonHashed;

      /// Clone the 'rhs'.                                                    
      /// Assumes all indirections are ordinary pointers, and is thus faster. 
      template<CT::Container C, CT::NoIntent IT>
      void EmplaceByCloningStandardPointers(this C& self, IT const& rhs) {
         static_assert(CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. GetHandle() first?");

         constexpr bool has_entries = requires { self.GetEntries(); };
         [[maybe_unused]] DMeta T;
         // If T is Text**, then dst/src are Text***                    
         void** dst = static_cast<void**>(self.GetHeapInnerAsVoid());
         void** src;
         
         if constexpr (CT::Handle<IT>) {
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
               T = rhs.GetTypeInner();
               LglsAssumeDev(self.IsSame(T), "Type mismatch");               
            }
            else static_assert(Same<TypeOf<C>, TypeOf<IT>>, "Type mismatch");
            src = static_cast<void**>(rhs.GetHeapInnerAsVoid());
         }
         else {
            if constexpr (CT::TypeErased<C>) {
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
               T = self.GetTypeInner();
            }
            else static_assert(Same<TypeOf<C>, IT>, "Type mismatch");   
            src = static_cast<void**>(const_cast<void*>(static_cast<const void*>(&rhs)));
         }

         if constexpr (CT::TypeErased<C>) {
            const size_t indirects = T.GetIndirections();
            if (indirects > 0) {
               // Allocate the origin first                             
               const auto originT = T.GetOrigin();
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  const auto cloned_origin = Allocator::Allocate(
                     originT,
                     pot_t(Roof2(originT.GetSize()))
                  );
               #else
                  const auto cloned_origin = Allocator::Allocate(
                     originT.GetAlignment(),
                     pot_t(Roof2(originT.GetSize()))
                  );
               #endif
               LglsAssert(cloned_origin, "Out of memory");

               // If T is Text**, ent is Allocation*[2]                 
               [[maybe_unused]] Allocation const* const* ent;
               if constexpr (has_entries)
                  ent = self.GetEntriesInner();
               
               if (indirects > 1) {
                  // Allocate multiple indirections                     
                  // If T is Text**, we allocate one intermediate ptr   
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     const auto cloned_ptrs = Allocator::Allocate(
                        T,
                        pot_t(Roof2(T.GetSize() * (indirects - 1)))
                     );
                  #else
                     const auto cloned_ptrs = Allocator::Allocate(
                        T.GetAlignment(),
                        pot_t(Roof2(T.GetSize() * (indirects - 1)))
                     );
                  #endif
                  
                  if (not cloned_ptrs) {
                     Allocator::Deallocate(cloned_origin);
                     LglsError("Out of memory");
                     return;
                  }
                  
                  cloned_ptrs->AddRef(indirects - 2);

                  // Given dst being Text***, we have:                  
                  //    *dst = cloned_ptrs                              
                  //   **dst = cloned_origin                            
                  //  ***dst = ***src                                   
                  void** ptr = static_cast<void**>(static_cast<void*>(cloned_ptrs->GetBlockStart()));
                  *dst = ptr;

                  if constexpr (has_entries)
                     DecvqAllCast(*ent) = cloned_ptrs;

                  T = T.GetDeptr();

                  do {
                     // Chain all intermediate pointers                 
                     src = static_cast<void**>(*src);
                     dst = static_cast<void**>(*dst);
                     T = T.GetDeptr();
                     *dst = dst + 1;

                     if constexpr (has_entries) {
                        ++ent;
                        DecvqAllCast(*ent) = cloned_ptrs;
                     }
                  }
                  while (T.IsSparse());
               }
               else T = T.GetDeptr();

               // The last indirection points to the cloned origin      
               *dst = cloned_origin->GetBlockStart();
               src = static_cast<void**>(*src);
               dst = static_cast<void**>(*dst);

               if constexpr (has_entries)
                  DecvqAllCast(*ent) = cloned_origin;
            }

            // Finally, clone inside the allocated origin               
            T.GetCloneConstructor()(src, dst);
         }
         else {
            //                                                          
            // Both sides are statically-typed and we can benefit       
            // from a lot of compile-time optimizations.                
            using T = TypeOf<C>;
            constexpr size_t indirects = IndirectsOf<T>;
            if constexpr (indirects > 0) {
               // Clone the origin first                                
               using originT = Decay<T>;
               #if LANGULUS_FEATURE(MANAGED_MEMORY)
                  const auto cloned_origin = Allocator::Allocate(
                     MetaDataOf<originT>(),
                     pot_t(Roof2(sizeof(originT)))
                  );
               #else
                  const auto cloned_origin = Allocator::Allocate(
                     pot_t(alignof(originT)),
                     pot_t(Roof2(sizeof(originT)))
                  );
               #endif
               LglsAssert(cloned_origin, "Out of memory");
               [[maybe_unused]] Allocation const* const* ent;
               if constexpr (has_entries)
                  ent = self.GetEntries();

               if constexpr (indirects > 1) {
                  // Multiple indirections                              
                  #if LANGULUS_FEATURE(MANAGED_MEMORY)
                     const auto cloned_ptrs = Allocator::Allocate(
                        MetaDataOf<T>(),
                        pot_t(Roof2(sizeof(T) * (indirects - 1)))
                     );
                  #else
                     const auto cloned_ptrs = Allocator::Allocate(
                        pot_t(alignof(T)),
                        pot_t(Roof2(sizeof(T) * (indirects - 1)))
                     );
                  #endif
                  
                  if (not cloned_ptrs) {
                     Allocator::Deallocate(cloned_origin);
                     LglsError("Out of memory");
                  }
                  cloned_ptrs->AddRef(indirects - 2);

                  // Given dst being Text***, we have:                  
                  //    *dst = cloned_ptrs                              
                  //   **dst = cloned_origin                            
                  //  ***dst = ***src                                   
                  void** ptr = static_cast<void**>(static_cast<void*>(cloned_ptrs->GetBlockStart()));
                  *dst = ptr;
                  if constexpr (has_entries)
                     DecvqAllCast(*ent) = cloned_ptrs;

                  ForEachIndirection<Deptr<T>>([&src, &dst, &ent, &cloned_ptrs] {
                     // Chain all intermediate pointers                 
                     src = static_cast<void**>(*src);
                     dst = static_cast<void**>(*dst);

                     *dst = dst + 1;

                     if constexpr (has_entries) {
                        ++ent;
                        DecvqAllCast(*ent) = cloned_ptrs;
                     }
                  });
               }
               
               // The last indirection points to the cloned origin      
               *dst = cloned_origin->GetBlockStart();
               src = static_cast<void**>(*src);
               dst = static_cast<void**>(*dst);
               if constexpr (has_entries)
                  DecvqAllCast(*ent) = cloned_origin;
            }

            IntentNew(dst, Clone(*static_cast<Decay<T>*>(static_cast<void*>(src))));
         }
      }

   #if LANGULUS_FEATURE(MANAGED_MEMORY)
      /// Clone the 'rhs'.                                                    
      /// This is a more generic approach that is considerably slower.        
      //TODO could benefit from static optimization                          
      template<CT::Container C, CT::NoIntent IT>
      void EmplaceByCloningCustomPointers(this C& self, IT const& rhs) {
         static_assert(CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. GetHandle() first?");

         void const* src_origin;         
         if constexpr (CT::Handle<IT>) {
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>)
               LglsAssumeDev(self.IsSame(rhs.GetTypeInner()), "Type mismatch");
            else
               static_assert(Same<TypeOf<C>, TypeOf<IT>>, "Type mismatch");
            
            src_origin = rhs.GetDense().GetRaw();
         }
         else {
            if constexpr (CT::TypeErased<C>)
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
            else
               static_assert(Same<TypeOf<C>, IT>, "Type mismatch");
            
            src_origin = static_cast<const void*>(&DenseCast(rhs));
         }

         // Clone the origin first                                      
         const DMeta T = self.GetType();
         auto indirections = T.GetIndirections();

         if (indirections) {
            // Containing sparse data                                   
            DMeta prev_type = T.GetDeptr(indirections - 1);
            DMeta type = T.GetOrigin();
            auto cloned = Allocator::AllocatePackedInner(
               prev_type.GetPointerSpecification(),
               type, pot_t(Roof2(type.GetSize()))
            );
            LglsAssert(cloned, "Out of memory");

            type.GetCloneConstructor()(
               const_cast<void*>(src_origin),
               cloned->GetBlockStart()
            );
            --indirections;

            // Then clone all indirection layers in reverse order       
            [[maybe_unused]] EntryPtr entries;
            if constexpr (CT::DeeplyOwned<C>) {
               entries = self.GetEntriesInner();
               DecvqAllCast(entries[indirections]) = cloned;
            }
         
            auto next_pointer = cloned->GetBlockStartPacked(prev_type.GetPointerSpecification());
            while (indirections) {
               type = prev_type;
               prev_type = T.GetDeptr(indirections - 1);
               cloned = Allocator::AllocatePackedInner(
                  prev_type.GetPointerSpecification(),
                  type, pot_t(Roof2(type.GetSize()))
               );

               // Chain the pointers                                    
               memcpy(cloned->GetBlockStart(), &next_pointer, type.GetSize());
               next_pointer = cloned->GetBlockStartPacked(prev_type.GetPointerSpecification());
               --indirections;

               // Save the new indirection allocation                   
               if constexpr (CT::DeeplyOwned<C>) {
                  DecvqAllCast(entries[indirections]) = cloned;
               }
            }

            // The final indirection is stored in mHeap                 
            memcpy(self.GetHeapInnerAsVoid(), &next_pointer, T.GetSize());
         }
         else {
            // Containing dense data                                    
            T.GetCloneConstructor()(
               const_cast<void*>(src_origin),
               self.GetHeapInner()
            );
         }
      }
   #endif

      /// Emplace on top of the first element using an intent                 
      ///   @attention Assumes destination memory has been preallocated,      
      ///      including all levels of indirection.                           
      ///   @attention Does not modify any container state.                   
      ///   @attention This overwrites previous handle without dereferencing  
      ///      it and without destroying anything.                            
      ///   @param intent constructor argument. If this container             
      ///      is statically typed, this can be any constructor argument,     
      ///      otherwise it has to be an instance of the contained type.      
      template<CT::Container C, CT::Intent I>
      void EmplaceWithIntent(this C&& self, I&& intent) {
         static_assert(CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. GetHandle() first?");
         using IT = Decvq<Deref<TypeOf<I>>>;
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");
         decltype(auto) rhs = LglsFwd(intent.what);
         static_assert(not CT::Copied<I>,
            "Since this function assumes container has been preallocated, "
            "it makes no sense to copy here - it should be handled outside this call."
         );

         if constexpr (CT::Cloned<I>) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               self.EmplaceByCloningCustomPointers(rhs);
            #else
               self.EmplaceByCloningStandardPointers(rhs);
            #endif
         }
         else if constexpr (CT::Handle<IT>) {
            // We're emplacing using a handle, which can be faster due  
            // to carrying allocation data with itself when sparse,     
            // instead of searching for it when having DeepOwnership.   
            // Doesn't matter if managed memory is disabled.            
            if constexpr (CT::TypeErased<C, IT>) {
               //                                                       
               // Either this container or the handle is type-erased    
               auto T = rhs.GetType();
               LglsAssumeDev(self.IsSame(T), "Type mismatch");
               auto src = const_cast<void*>(static_cast<const void*>(rhs.GetRaw()));
               auto dst = self.GetRaw();
               
               if constexpr (CT::Moved<I>) {
                  if (rhs.IsConstant())
                     T.GetReferConstructor()(src, dst);
                  else
                     T.GetMoveConstructor()(src, dst);
               }
               else if constexpr (CT::Abandoned<I>) {
                  if (rhs.IsConstant())
                     T.GetReferConstructor()(src, dst);
                  else
                     T.GetAbandonConstructor()(src, dst);
               }
               else if constexpr (CT::Referred<I>)
                  T.GetReferConstructor()(src, dst);
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownConstructor()(src, dst);
               else
                  static_assert(false, "Unrecognized intent");

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
            else {
               //                                                       
               // Both sides are statically-typed and we can benefit    
               // from a lot of compile-time optimizations.             
               if constexpr (CT::Typed<C, IT>)
                  static_assert(Same<TypeOf<C>, TypeOf<IT>>, "Type mismatch");
               else
                  LglsAssumeDev(self.IsSame(rhs), "Type mismatch");
               using T = Tif<CT::Typed<C>, TypeOf<C>, TypeOf<IT>>;

               if constexpr (CT::Mutable<T> or not I::IsMoved())
                  IntentNew(self.GetHeapInner(), I::Nest(*rhs.template GetRawAs<T>()));
               else
                  IntentNew(self.GetHeapInner(), Refer(*rhs.template GetRawAs<T>()));

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
         }
         else {
            if constexpr (CT::TypeErased<C, IT>) {
               //                                                       
               // This container is type-erased                         
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
               auto T = self.GetType();
               const auto src = const_cast<void*>(static_cast<const void*>(&rhs));
               const auto dst = self.GetRaw();
               
               if constexpr (CT::Moved<I>)
                  T.GetMoveConstructor()(src, dst);
               else if constexpr (CT::Abandoned<I>)
                  T.GetAbandonConstructor()(src, dst);
               else if constexpr (CT::Referred<I>)
                  T.GetReferConstructor()(src, dst);
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownConstructor()(src, dst);
               else
                  static_assert(false, "Unrecognized intent");

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
            else {
               //                                                       
               // This container is statically-typed                    
               if constexpr (CT::Typed<C>)
                  static_assert(Same<TypeOf<C>, IT>, "Type mismatch");
               else
                  LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");

               IntentNew(self.GetHeapInnerAsVoid(), LglsFwd(intent));

               if_available(self.EmplaceEntries(LglsFwd(intent)));
            }
         }
      }

      enum class AllocationStrategy {
         NoStateChange,
         TypeAndFreshAllocate,
         TypeAndReallocate
      };
      
      /// Emplace a new default-constructed item at the first position.       
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @attention doesn't modify count                                   
      template<AllocationStrategy STRAT = AllocationStrategy::TypeAndFreshAllocate, class E = void, CT::Container C>
      void EmplaceDefault(this C& self) {
         static_assert(CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. GetHandle() first?");

         if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
            if_available(self.ResetState());
         }

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // This container is type-erased                            
            if constexpr (STRAT != AllocationStrategy::NoStateChange) {
               if constexpr (CT::NotVoid<E>)
                  self.template SetType<E>();
            }

            if constexpr (CT::NotVoid<E>) {
               // The type we're constructing is statically known       
               static_assert(CT::Defaultable<E>,
                  "Contained type is not default-constructible");
               LglsAssumeDev(self.template IsSame<E>(), "Type mismatch");

               // Allocate if we have to                                
               if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate)
                  self.AllocateFresh(self.RequestHeap(1));
               else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate)
                  self.AllocateMore(1);

               // Construct the first element                           
               new (self.GetRaw()) E {};

               if constexpr (CT::Sparse<E>)
                  if_available(*self.GetEntries() = nullptr);
            }
            else {
               // The type we're constructing isn't statically known    
               auto T = self.GetTypeInner();
               auto constructor = T.GetDefaultConstructor();
               LglsAssert(constructor,
                  "Contained type is not default-constructible");

               // Allocate if we have to. Do it only after we're sure   
               // that construction is possible                         
               if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate)
                  self.AllocateFresh(self.RequestHeap(1));
               else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate)
                  self.AllocateMore(1);

               // Construct the first element                           
               constructor(self.GetRaw());

               if constexpr (requires { self.GetEntries(); }) {
                  if (T.IsSparse())
                     *self.GetEntries() = nullptr;
               }
            }
         }
         else {
            //                                                          
            // This container is statically-typed. E is ignored.        
            // Allocate if we have to                                   
            if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
               self.GetType();
               self.AllocateFresh(self.RequestHeap(1));
            }
            else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate) {
               self.GetType();
               self.AllocateMore(1);
            }

            // Construct the first element                              
            using T = TypeOf<C>;
            LglsAssumeDev(self.template IsSame<T>(), "Type mismatch");
            new (self.GetRaw()) T {};
            
            if constexpr (CT::Sparse<T>)
               if_available(*self.GetEntries() = nullptr);
         }

         // Update count                                                
         if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
            if_available(self.SetCountInner(1));
         }
         else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate
         and requires { self.SetCountInner(1); }) {
            if (self.IsEmpty())
               self.SetCountInner(1);
         }

         // Update hash                                                 
         if_available(self.SetHashInner(0));
      }

      /// Emplace a new manually constructed item at the first position.      
      /// If zero arguments were provided, this will EmplaceDefault.          
      /// Supports describe-construction and handles.                         
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      template<AllocationStrategy STRAT = AllocationStrategy::TypeAndFreshAllocate, class E = void, CT::Container C, class...A>
      void EmplaceConstruct(this C& self, A&&...arguments) {
         static_assert(STRAT != AllocationStrategy::NoStateChange or CT::ContainsOne<C>,
            "Emplacing only first element in a container with many. GetHandle() first?");
         static_assert(sizeof...(A) > 0,
            "No arguments - use EmplaceDefault instead");

         if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
            if_available(self.ResetState());
         }

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // This container is type-erased                            
            if constexpr (sizeof...(A) == 1) {
               using A1 = typename Types<A...>::First;

               // Set type if we have to                                
               if constexpr (STRAT != AllocationStrategy::NoStateChange) {
                  if constexpr (CT::NotVoid<E>)
                     self.template SetType<E>();
                  else if constexpr (CT::Handle<A1>)
                     self.SetType(DeintCast(arguments...).GetType());
                  else if constexpr (not Same<A1, Describe>)
                     self.SetType(MetaDataOf<Decvq<Deref<Deint<A1>>>>());
               }

               if constexpr (Same<A1, Describe>) {
                  // Describe-construct first element                   
                  if constexpr (CT::NotVoid<E>) {
                     // The type we're describing is statically known   
                     static_assert(CT::Dense<E>,
                        "Describe-construction works only for dense data");
                     static_assert(CT::DescribeConstructible<E>,
                        "Contained type is not describe-constructible");
                     LglsAssumeDev(self.template IsSame<E>(), "Type mismatch");

                     // Allocate if we have to                          
                     if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate)
                        self.AllocateFresh(self.RequestHeap(1));
                     else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate)
                        self.AllocateMore(1);

                     new (self.GetRaw()) E {LglsFwd(arguments)...};
                  }
                  else {
                     // The type we're describing isn't known statically
                     auto T = self.GetTypeInner();
                     LglsAssert(T.IsDense(),
                        "Describe-construction works only for dense data");
                     auto constructor = T.GetDescribeConstructor();
                     LglsAssert(constructor,
                        "Contained type is not describe-constructible");

                     // Allocate if we have to. Do it only after we're  
                     // sure that construction is possible              
                     if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate)
                        self.AllocateFresh(self.RequestHeap(1));
                     else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate)
                        self.AllocateMore(1);

                     // Describe-construct the first element            
                     constructor(self.GetRaw(), LglsFwd(arguments.what)...);
                  }
               }
               else {
                  // Allocate if we have to                             
                  if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate)
                     self.AllocateFresh(self.RequestHeap(1));
                  else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate)
                     self.AllocateMore(1);

                  // Construct the first element                        
                  if constexpr (CT::Copied<IntentOf(arguments)...>)
                     self.GetHandle().EmplaceWithIntent(Refer(LglsFwd(arguments))...);
                  else
                     self.GetHandle().EmplaceWithIntent(FWDIntent(arguments)...);
               }
            }
            else if constexpr (CT::NotVoid<E>) {
               // Set type if we have to                                
               if constexpr (STRAT != AllocationStrategy::NoStateChange)
                  self.template SetType<E>();

               // Construct the first element                           
               if constexpr (CT::Dense<E>)
                  self.EmplaceWithIntent(Abandon{E {LglsFwd(arguments)...}});
               else static_assert(false,
                  "Too many arguments for emplacing a sparse instance");
            }
            else static_assert(false,
               "Too many arguments for emplacing in a type-erased container. "
               "You should either provide E type, or set pack type and pack "
               "all arguments inside a Describe intent first."
            );
         }
         else {
            //                                                          
            // This container is statically-typed. E is ignored.        
            // Allocate if we have to                                   
            if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
               self.GetType();
               self.AllocateFresh(self.RequestHeap(1));
            }
            else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate) {
               self.GetType();
               self.AllocateMore(1);
            }

            // Construct the first element                              
            using T = TypeOf<C>;
            if constexpr (sizeof...(A) == 1 and (CT::Sparse<T> or Same<T, Deint<A>...>)) {
               if constexpr (CT::Copied<IntentOf(arguments)...>)
                  self.GetHandle().EmplaceWithIntent(Refer(LglsFwd(arguments))...);
               else
                  self.GetHandle().EmplaceWithIntent(FWDIntent(arguments)...);
            }
            else if constexpr (CT::Dense<T>)
               self.EmplaceWithIntent(Abandon {Decvq<T> {LglsFwd(arguments)...}});
            else static_assert(false,
               "Too many arguments for emplacing a sparse instance");
         }
         
         // Update count                                                
         if constexpr (STRAT == AllocationStrategy::TypeAndFreshAllocate) {
            if_available(self.SetCountInner(1));
         }
         else if constexpr (STRAT == AllocationStrategy::TypeAndReallocate
         and requires { self.SetCountInner(1); }) {
            if (self.IsEmpty())
               self.SetCountInner(1);
         }

         // Update hash                                                 
         if_available(self.SetHashInner(0));
      }

   public:
      /// Generic emplacement that constructs/overwrites specific element.    
      /// Any overwritten element will be dereferenced/destroyed first.       
      ///   @tparam E Sets the type of the container if empty. Ignored if     
      ///      container is statically-typed.                                 
      ///   @param at The index at which to emplace                           
      ///   @param arguments Constructor arguments for initializing an        
      ///      element. If C is type-erased, argument must be Describe.       
      ///   @return a reference or handle to the newly created element        
      template<class E = void, CT::ContainsMany C, class...A>
      auto EmplaceAt(this C& self, CT::Index auto&& at, A&&...arguments)
      -> DecidePick<C> requires CT::IndexedLinearly<C> /*requires CT::RangeEmplaceable<C, A...>*/ {
         DecidePick<C> pick = self.template AsAt<DecidePick<C>>(LglsFwd(at));
         pick.Emplace(LglsFwd(arguments)...);
         return pick;
      }

      /// Generic emplacement that constructs/overwrites the first element.   
      /// Any overwritten element will be dereferenced/destroyed first.       
      ///   @tparam E Sets the type of the container if empty. Ignored if     
      ///      container is statically-typed.                                 
      ///   @param arguments Constructor arguments                            
      ///   @return a reference or handle to the newly created element        
      template<class E = void, CT::Container C, class...A>
      auto Emplace(this C& self, A&&...arguments)
      -> DecidePick<C> /*requires CT::RangeEmplaceable<C, A...>*/ {
         auto a = self.GetAllocation();
         if (not a) {
            // No ownership, just fresh-allocate                        
            try {
               if constexpr (sizeof...(arguments) > 0)
                  self.template EmplaceConstruct<AllocationStrategy::TypeAndFreshAllocate, E>(LglsFwd(arguments)...);
               else
                  self.template EmplaceDefault<AllocationStrategy::TypeAndFreshAllocate, E>();
            }
            catch (...) {
               // Reset heap count in case 'self' was disowned          
               if_available(self.SetReservedInner(0));
               if_available(self.SetHashTableInner(nullptr));
               self.ResetCount();
               throw;
            }
         }
         else if (self.IsEmpty()) {
            // The container is empty, but an allocation is available   
            if (a->GetUses() != 1) {
               // We're not the only owner of this memory.              
               // We have to branch off with a fresh allocation.        
               DecvqAllCast(a)->AddRef(-1);

               try {
                  if constexpr (sizeof...(arguments) > 0)
                     self.template EmplaceConstruct<AllocationStrategy::TypeAndFreshAllocate, E>(LglsFwd(arguments)...);
                  else
                     self.template EmplaceDefault<AllocationStrategy::TypeAndFreshAllocate, E>();
               }
               catch (...) {
                  self.SetAllocationInner(nullptr);
                  if_available(self.SetReservedInner(0));
                  if_available(self.SetHashTableInner(nullptr));
                  self.ResetCount();
                  throw;
               }
            }
            else {
               // Emplace a new element on the first position.          
               // We're allowed to reuse the memory.                    
               if constexpr (sizeof...(arguments) > 0)
                  self.template EmplaceConstruct<AllocationStrategy::TypeAndReallocate, E>(LglsFwd(arguments)...);
               else
                  self.template EmplaceDefault<AllocationStrategy::TypeAndReallocate, E>();
            }
         }
         else {
            // The container is not empty                               
            if (a->GetUses() != 1) {
               // We're not the only owner of this memory.              
               // We have to branch off with a fresh allocation.        
               self.Free();

               try {
                  if constexpr (sizeof...(arguments) > 0)
                     self.template EmplaceConstruct<AllocationStrategy::TypeAndFreshAllocate, E>(LglsFwd(arguments)...);
                  else
                     self.template EmplaceDefault<AllocationStrategy::TypeAndFreshAllocate, E>();
               }
               catch (...) {
                  self.SetAllocationInner(nullptr);
                  if_available(self.SetReservedInner(0));
                  if_available(self.SetHashTableInner(nullptr));
                  self.ResetCount();
                  throw;
               }
            }
            else {
               // We're allowed to reuse the memory.                    
               // Need to destroy and overwrite only the first element. 
               auto item = self.GetHandle();
               item.DestroyElement();
               if_available(item.ResetEntries());
               //TODO clear the correspnding hash table spot?

               // Emplace a new element on the first position.          
               // Any state change is forbidden - container is full.    
               try {
                  if constexpr (sizeof...(arguments) > 0)
                     item.template EmplaceConstruct<AllocationStrategy::NoStateChange, E>(LglsFwd(arguments)...);
                  else
                     item.template EmplaceDefault<AllocationStrategy::NoStateChange, E>();
               }
               catch (...) {
                  // If emplacement fails, we are forced to destroy     
                  // all remaining elements as well.                    
                  if constexpr (CT::ContainsMany<C>) {
                     item += 1;
                     const auto itemsEnd = self.GetHandle() + self.GetCount();
                     while (item.GetRaw() != itemsEnd.GetRaw()) {
                        item.DestroyElement();
                        ++item;
                     }
                  }
                  Allocator::Deallocate(DecvqAllCast(a));
                  self.SetAllocationInner(nullptr);
                  if_available(self.SetReservedInner(0));
                  if_available(self.SetHashTableInner(nullptr));
                  self.ResetCount();
                  throw;
               }
            }
         }

         // Return a reference/handle to the newly emplaced element     
         return self.template As<Deref<DecidePick<C>>>();
      }
   };
}
