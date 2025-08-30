///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "Indexed-Linear.hpp"


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
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using PickMut = typename Deref<C>::PickMut;
      
      /// Emplace a new item at the first element using an intent             
      ///   @attention assumes destination memory has been preallocated,      
      ///      including all levels of indirection                            
      ///   @attention does not modify any container state                    
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param intent - constructor argument. If this container           
      ///      is statically typed, this can be any constructor argument,     
      ///      otherwise it has to be an instance of the container type       
      template<CT::Container C, CT::Intent I>
      void EmplaceWithIntent(this C& self, I&& intent) {
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

               if constexpr (CT::Moved<I>)
                  T.GetMoveConstructor()(self.GetRaw(), rhs.GetRaw());
               else if constexpr (CT::Abandoned<I>)
                  T.GetAbandonConstructor()(self.GetRaw(), rhs.GetRaw());
               else if constexpr (CT::Referred<I>)
                  T.GetReferConstructor()(self.GetRaw(), rhs.GetRaw());
               else if constexpr (CT::Copied<I>)
                  T.GetCopyConstructor()(self.GetRaw(), rhs.GetRaw());
               else if constexpr (CT::Disowned<I>)
                  T.GetDisownConstructor()(self.GetRaw(), rhs.GetRaw());
               else if constexpr (CT::Cloned<I>)
                  T.GetCloneConstructor()(self.GetRaw(), rhs.GetRaw());
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
               IntentNew(self.GetHeapInner(), I::Nest(*rhs.GetRaw()));
            }
         }
         else if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            LglsAssumeDev(CT::Dense<IT>, "Sparseness mismatch");
            LglsAssumeDev(self.template IsSimilar<IT>(), "Type mismatch");
            auto T = self.GetTypeInner();

            if constexpr (CT::Moved<I>)
               T.GetMoveConstructor()(self.GetRaw(), &rhs);
            else if constexpr (CT::Abandoned<I>)
               T.GetAbandonConstructor()(self.GetRaw(), &rhs);
            else if constexpr (CT::Referred<I>)
               T.GetReferConstructor()(self.GetRaw(), &rhs);
            else if constexpr (CT::Copied<I>)
               T.GetCopyConstructor()(self.GetRaw(), &rhs);
            else if constexpr (CT::Disowned<I>)
               T.GetDisownConstructor()(self.GetRaw(), &rhs);
            else if constexpr (CT::Cloned<I>)
               T.GetCloneConstructor()(self.GetRaw(), &rhs);
            else
               static_assert(false, "Unrecognized intent");
         }
         else {
            //                                                          
            // This container is statically-typed                       
            using T = TypeOf<C>;
            static_assert(CT::Similar<T, IT>, "Type mismatch");
            IntentNew(self.GetHeapInner(), FWD(intent));
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

         if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            auto T = self.GetTypeInner();
            T.GetDefaultConstructor()(self.GetRaw());
         }
         else {
            //                                                          
            // This container is statically-typed                       
            using T = TypeOf<C>;
            new (self.GetRaw()) T {};
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
      template<CT::Container C>
      void EmplaceConstruct(this C& self, auto&&...arguments) {
         if constexpr (sizeof...(arguments) == 0)
            self.EmplaceDefault();
         else {
            LglsAssumeDev(self.GetRaw(), "Invalid heap");
            LglsAssumeDev(self.IsTyped(), "Invalid type");

            if constexpr (C::TypeErased) {
               //                                                       
               // This container is type-erased                         
               auto T = self.GetType();
               T.GetDescribeConstructor()(self.GetRaw(), {FWD(arguments)...});
            }
            else {
               //                                                       
               // This container is statically-typed                    
               using T = TypeOf<C>;
               new (const_cast<void*>(self.GetHeapInner())) T {FWD(arguments)...};
            }
         }
      }

   public:
      /// Generic emplacement that constructs/overwrites specific element.    
      /// Any overwritten element will be dereferenced/destroyed first.       
      template<CT::IndexedLinearly C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;

      /// Generic emplacement that constructs/overwrites the first element.   
      /// Any overwritten element will be dereferenced/destroyed first.       
      template<CT::Container C, class...A>
      auto Emplace(this C& self, A&&...arguments) -> PickMut<C>
      requires CT::RangeEmplaceable<C, A...> {
         if (self.IsEmpty())
            self.AllocateMore(1);
         else
            self.template DestroyElement<false>();
         
         self.EmplaceConstruct(FWD(arguments)...);

         if constexpr (requires { self.SetCountInner(1); }) {
            if (self.IsEmpty())
               self.SetCountInner(1);
         }

         return self.template GetAs<PickMut<C>>();
      }
   };
}
