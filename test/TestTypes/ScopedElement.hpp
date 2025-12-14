///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Typenav.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/Allocator.hpp>
#include <Langulus/MetaOf.hpp>


/// Useful for creating instances of types on the heap, with multiple levels  
/// of indirection                                                            
template<class T, bool MANAGED = false>
struct ScopedElement {
   using CTTI_ReflectAs = void;
   using Type = T;
   using Allocation = Langulus::Allocation;
   using Allocator = Langulus::Allocator;
   
   T* element = nullptr;
   Allocation* entries[Langulus::IndirectsOf<T> + 1] = {};

protected:
   template<class INNER, class...A>
   static void NestedConstructor(INNER*& place, Allocation** entry, A&&...arguments) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         if constexpr (MANAGED) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               *entry = Allocator::Allocate(Langulus::MetaDataOf<INNER>(), pot_t(Roof2(sizeof(INNER))));
            #else
               *entry = Allocator::Allocate(pot_t(alignof(INNER)), pot_t(Roof2(sizeof(INNER))));
            #endif

            place = reinterpret_cast<INNER*>((*entry)->GetBlockStart());

            if constexpr (requires { new INNER{ FWD(arguments)... }; })
               new (place) INNER{ FWD(arguments)... };
            else if constexpr (requires { new INNER{ INNER::FromNumber(FWD(arguments)...) }; })
               new (place) INNER{ INNER::FromNumber(FWD(arguments)...) };
            else
               static_assert(false, "Unable to construct");
         }
         else {
            if constexpr (requires { new INNER{ FWD(arguments)... }; })
               place = new INNER{ FWD(arguments)... };
            else if constexpr (requires { new INNER{ INNER::FromNumber(FWD(arguments)...) }; })
               place = new INNER{ INNER::FromNumber(FWD(arguments)...) };
            else
               static_assert(false, "Unable to construct");
         }
      }
      else {
         if constexpr (MANAGED) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               *entry = Allocator::Allocate(Langulus::MetaDataOf<INNER>(), pot_t(Roof2(sizeof(INNER))));
            #else
               *entry = Allocator::Allocate(pot_t(alignof(INNER)), pot_t(Roof2(sizeof(INNER))));
            #endif

            place = reinterpret_cast<INNER*>((*entry)->GetBlockStart());
         }
         else {
            place = new INNER{ nullptr };
         }

         NestedConstructor(*place, entry + 1, FWD(arguments)...);
      }
   }
   
   template<class INNER>
   static void NestedDestructor(INNER* place, Allocation** entry) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         #if not LANGULUS_FEATURE(NEWDELETE)
            if constexpr (CT::Referenced<INNER>)
               place->Reference(-1);
         #endif

         if (not *entry)
            delete place;
         else if constexpr (MANAGED) {
            LglsAssumeDev((*entry)->GetUses() >= 1);
            if ((*entry)->GetUses() == 1) {
               if constexpr (requires { place->~INNER(); })
                  place->~INNER();
               Allocator::Deallocate(*entry);
            }
            else (*entry)->AddRef(-1);
         }
      }
      else if (place) {
         NestedDestructor(*place, entry + 1);

         if (not *entry)
            delete place;
         else if constexpr (MANAGED) {
            LglsAssumeDev((*entry)->GetUses() >= 1);
            if ((*entry)->GetUses() == 1)
               Allocator::Deallocate(*entry);
            else
               (*entry)->AddRef(-1);
         }
      }
   }

public:
   template<class...A>
   ScopedElement(A&&...arguments) {
      NestedConstructor(element, entries, FWD(arguments)...);
   }
   
   ~ScopedElement() {
      NestedDestructor(element, entries);
   }

   auto operator *  ()       -> T&       {return *element;}
   auto operator *  () const -> T const& {return *element;}
   auto operator -> ()       -> T*       {return  element;}
   auto operator -> () const -> T const* {return  element;}
};
