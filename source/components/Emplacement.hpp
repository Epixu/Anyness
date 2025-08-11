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
//#include "DeepOwnership.hpp"
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Resolvable.hpp>


namespace Langulus::CT
{
   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible         
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
   /// Implements emplacement for containers                                  
   ///   @tparam ID - heap we're inserting to                                 
   ///                                                                        
   template<unsigned ID = 0>
   struct Emplacement {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   protected:
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using PickMut = typename Deref<C>::PickMut;
      
      /// Emplace a new item at the first element, with or without an intent  
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
               auto T = rhs.GetType();
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
               IntentNew(self.GetRaw(), I::Nest(*rhs.GetRaw()));
            }
         }
         else if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            LglsAssumeDev(CT::Dense<IT>, "Sparseness mismatch");
            LglsAssumeDev(self.template IsSimilar<IT>(), "Type mismatch");
            auto T = self.GetType();

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
            IntentNew(self.GetRaw(), FWD(intent));
         }
      }

   public:
      /// Emplacement at specific index                                       
      template<CT::IndexedLinearly C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;

      /// Generic emplacement                                                 
      template<CT::Container C, class...A>
      auto Emplace(this C&, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;
   };
}
