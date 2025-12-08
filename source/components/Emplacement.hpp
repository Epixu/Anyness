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
#include "Langulus/CT/Describable.hpp"


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

namespace Langulus::Anyness::Component
{

   template<class T>
   void ForEachIndirection(auto&& lambda) {
      if constexpr (CT::Sparse<T>) {
         lambda();
         ForEachIndirection<Deptr<T>>(FWD(lambda));
      }
   }
   
   ///                                                                        
   /// Implements emplacement for containers.                                 
   /// Unlike insertion, emplacement reuses the same memory space and         
   /// guarantees that nothing moves around.                                  
   ///   @tparam ID - heap we're emplacing to                                 
   template<unsigned ID>
   struct Emplacement {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   protected:
      template<unsigned> friend struct HeapMovable;
      template<unsigned, class> friend struct Insertion;
      
      template<CT::Container C>
      using PickMut = typename Deref<C>::PickMut;

      /// Clone the 'rhs'                                                     
      template<CT::Container C, CT::NoIntent IT>
      void EmplaceByCloning(this C& self, IT const& rhs) {
         [[maybe_unused]] DMeta T;
         // If T is Text**, then dst/src are Text***                    
         void* dst = self.GetHeapInner();         
         void* src;
         
         if constexpr (CT::Handle<IT>) {
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
               T = rhs.GetTypeInner();
               LglsAssumeDev(self.IsSame(T), "Type mismatch");               
            }
            else static_assert(Same<TypeOf<C>, TypeOf<IT>>, "Type mismatch");
            src = const_cast<void*>(rhs.GetHeapInner());
         }
         else {
            if constexpr (CT::TypeErased<C>) {
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
               T = self.GetTypeInner();
            }
            else static_assert(Same<TypeOf<C>, IT>, "Type mismatch");   
            src = const_cast<void*>(static_cast<const void*>(&rhs));
         }

         if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
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
               auto ent = self.GetEntries();
               
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
                  }
                  cloned_ptrs->Keep(indirects - 2);

                  // Given dst being Text***, we have:                  
                  //    *dst = cloned_ptrs                              
                  //   **dst = cloned_origin                            
                  //  ***dst = ***src                                   
                  *static_cast<void**>(dst) = cloned_ptrs->GetBlockStart();
                  *ent = cloned_ptrs;

                  do {
                     // Chain all intermediate pointers                 
                     src = *static_cast<void**>(src); //TODO won't work for packed pointers
                     dst = *static_cast<void**>(dst);
                     ++ent;
                     T = T.GetDeptr();

                     *static_cast<void**>(dst) = static_cast<void**>(dst) + 1;
                     *ent = cloned_ptrs;
                  }
                  while (T.IsSparse());
               }
               else {
                  src = *static_cast<void**>(src);//TODO won't work for packed pointers
                  dst = *static_cast<void**>(dst);
                  T = T.GetDeptr();
               }

               // The last indirection points to the cloned origin      
               *static_cast<void**>(dst) = cloned_origin->GetBlockStart();
               *ent = cloned_origin;
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
               auto ent = self.GetEntries();
               
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
                  void** ptrs = reinterpret_cast<void**>(cloned_ptrs->GetBlockStart());
                  cloned_ptrs->Keep(indirects - 2);
                  *static_cast<void**>(dst) = *ptrs;

                  ForEachIndirection<T>([&ptrs, &src, &dst, &ent, &cloned_ptrs] {
                     *ptrs = ptrs + 1;                     
                     src = *static_cast<void**>(src);//TODO won't work for packed pointers
                     dst = *static_cast<void**>(dst);
                     *(ent++) = cloned_ptrs;                     
                  });
               }
               else {
                  src = *static_cast<void**>(src);//TODO won't work for packed pointers
                  dst = *static_cast<void**>(dst);
               }
               
               *static_cast<void**>(dst) = cloned_origin->GetBlockStart();
               *ent = cloned_origin;
            }

            IntentNew(dst, Clone(*static_cast<Decay<T>*>(src)));
         }
      }

      /// Emplace on top of the first element using an intent                 
      ///   @attention assumes destination memory has been preallocated,      
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param intent - constructor argument. If this container           
      ///      is statically typed, this can be any constructor argument,     
      ///      otherwise it has to be an instance of the contained type.      
      template<CT::Container C, CT::Intent I>
      void EmplaceWithIntent(this C& self, I&& intent) {
         using IT = Decvq<Deref<TypeOf<I>>>;
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");
         decltype(auto) rhs = FWD(intent.what);

         if constexpr (CT::Copied<I>)
            self.EmplaceByCopying(rhs);
         else if constexpr (CT::Cloned<I>)
            self.EmplaceByCloning(rhs);
         else if constexpr (CT::Handle<IT>) {
            // We're emplacing using a handle, which can be faster due  
            // to carrying allocation data with itself when sparse,     
            // instead of searching for it when having DeepOwnership.   
            // Doesn't matter if managed memory is disabled.            
            if constexpr (CT::TypeErased<C> or CT::TypeErased<IT>) {
               //                                                       
               // Either this container or the handle is type-erased    
               auto T = rhs.GetTypeInner();
               LglsAssumeDev(self.IsSame(T), "Type mismatch");
               auto src = const_cast<void*>(rhs.GetRaw());
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

               if (T.IsSparse()) {
                  if_available(self.EmplaceEntries(FWD(intent)));
               }
            }
            else {
               //                                                       
               // Both sides are statically-typed and we can benefit    
               // from a lot of compile-time optimizations.             
               using T = TypeOf<C>;
               static_assert(Same<T, TypeOf<IT>>, "Type mismatch");
               if constexpr (CT::Mutable<TypeOf<IT>> or not I::IsMoved())
                  IntentNew(self.GetHeapInner(), I::Nest(*rhs.GetRaw()));
               else
                  IntentNew(self.GetHeapInner(), Refer(*rhs.GetRaw()));

               if constexpr (CT::Sparse<T>) {
                  if_available(self.EmplaceEntries(FWD(intent)));
               }
            }
         }
         else {
            if constexpr (CT::TypeErased<C>) {
               //                                                       
               // This container is type-erased                         
               LglsAssumeDev(self.template IsSame<IT>(), "Type mismatch");
               auto T = self.GetTypeInner();
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

               if (T.IsSparse()) {
                  if_available(self.EmplaceEntries(FWD(intent)));
               }
            }
            else {
               //                                                       
               // This container is statically-typed                    
               using T = TypeOf<C>;
               static_assert(Same<T, IT>, "Type mismatch");
               IntentNew(self.GetHeapInner(), FWD(intent));

               if constexpr (CT::Sparse<T>) {
                  if_available(self.EmplaceEntries(FWD(intent)));
               }
            }
         }
      }
      
      /// Emplace a new default-constructed item at the first element         
      ///   @attention assumes destination memory has been preallocated,      
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      template<CT::Container C>
      void EmplaceDefault(this C& self) {
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // This container is type-erased                            
            auto T = self.GetTypeInner();
            T.GetDefaultConstructor()(self.GetRaw());
            
            if (T.IsSparse()) {
               if_available(*self.GetEntries() = nullptr);
            }
         }
         else {
            //                                                          
            // This container is statically-typed                       
            using T = TypeOf<C>;
            new (self.GetRaw()) T {};
            
            if constexpr (CT::Sparse<T>) {
               if_available(*self.GetEntries() = nullptr);
            }
         }
      }

      /// Emplace a new manually constructed item at the first element.       
      /// If zero arguments were provided, this will EmplaceDefault.          
      /// When C is type-erased, this will perform a describe-construction.   
      ///   @attention assumes destination memory has been preallocated,      
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      template<CT::Container C, class...A>
      void EmplaceConstruct(this C& self, A&&...arguments) {
         static_assert(sizeof...(A) > 0,
            "No arguments - use EmplaceDefault instead");      
         LglsAssumeDev(self.GetRaw(), "Invalid heap");
         LglsAssumeDev(self.IsTyped(), "Invalid type");

         if constexpr (CT::TypeErased<C>) {
            //                                                          
            // This container is type-erased                            
            auto T = self.GetType();
            LglsAssert(T.IsDense(),
               "EmplaceConstruct works only for dense containers");
            
            if constexpr (sizeof...(A) == 1) {
               using A1 = typename Types<A...>::First;
               if constexpr (Same<A1, Describe>)
                  T.GetDescribeConstructor()(self.GetRaw(), FWD(arguments.what)...);
               else
                  static_assert(false, "Argument must be a Describe instance");
            }
            else static_assert(false,
               "Too many arguments for emplacing a type-erased instance. "
               "You should group all arguments inside a Describe first"
            );
         }
         else {
            //                                                          
            // This container is statically-typed                       
            using T = TypeOf<C>;
            if constexpr (sizeof...(A) == 1 and (CT::Sparse<T> or Same<T, Deint<A>...>))
               self.EmplaceWithIntent(FWDIntent(arguments)...);
            else if constexpr (CT::Dense<T>)
               self.EmplaceWithIntent(Abandon {Decvq<T> {FWD(arguments)...}});
            else static_assert(false,
               "Too many arguments for emplacing a sparse instance");
         }
      }

   public:
      /// Generic emplacement that constructs/overwrites specific element.    
      /// Any overwritten element will be dereferenced/destroyed first.       
      template<CT::ContainsMany C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;

      /// Generic emplacement that constructs/overwrites the first element.   
      /// Any overwritten element will be dereferenced/destroyed first.       
      template<CT::Container C, class...A>
      auto Emplace(this C& self, A&&...arguments) -> PickMut<C>
      requires CT::RangeEmplaceable<C, A...> {
         if (self.IsEmpty())
            self.AllocateMore(1);
         else if constexpr (CT::DeeplyOwned<C>)
            self.DestroyElementDeep();
         else
            self.DestroyElement();

         if constexpr (sizeof...(arguments) > 0)
            self.EmplaceConstruct(FWD(arguments)...);
         else
            self.EmplaceDefault();

         if constexpr (requires { self.SetCountInner(1); }) {
            if (self.IsEmpty())
               self.SetCountInner(1);
         }

         return self.template As<PickMut<C>>();
      }
   };
}
