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
   using AllocationPtr = Langulus::AllocationPtr;
   using Allocator = Langulus::Allocator;
   static constexpr bool Managed = MANAGED;
   
   T* element = nullptr;
   AllocationPtr entries[Langulus::IndirectsOf<T> + 1] = {};

protected:
   template<class INNER, class...A>
   static void NestedConstructor(INNER*& place, AllocationPtr* entry, A&&...arguments) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         if constexpr (MANAGED) {
            #if LANGULUS_FEATURE(MANAGED_MEMORY)
               *entry = Allocator::Allocate(Langulus::MetaDataOf<INNER>(), pot_t(Roof2(sizeof(INNER))));
            #else
               *entry = Allocator::Allocate(pot_t(alignof(INNER)), pot_t(Roof2(sizeof(INNER))));
            #endif

            place = reinterpret_cast<INNER*>((*entry)->GetBlockStart());

            if constexpr (requires { new INNER (LglsFwd(arguments)...); })
               new (place) INNER (LglsFwd(arguments)...);
            else if constexpr (requires { new INNER (INNER::FromNumber(LglsFwd(arguments)...)); })
               new (place) INNER (INNER::FromNumber(LglsFwd(arguments)...));
            else
               static_assert(false, "Unable to construct");
         }
         else {
            if constexpr (requires { new INNER (LglsFwd(arguments)...); })
               place = new INNER (LglsFwd(arguments)...);
            else if constexpr (requires { new INNER (INNER::FromNumber(LglsFwd(arguments)...)); })
               place = new INNER (INNER::FromNumber(LglsFwd(arguments)...));
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

         NestedConstructor(*place, entry + 1, LglsFwd(arguments)...);
      }
   }
   
   template<class INNER>
   static void NestedDestructor(INNER* place, AllocationPtr* entry) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         int individual_refs = 0;
         if constexpr (CT::Referenced<INNER>)
            individual_refs = place->Reference(-1);

         if (not *entry) {
            LglsAssert(individual_refs == 0,
               "Unmanaged CT::Referenced instance memory was deleted before references reach zero"
               " - revise your test to avoid false positives."
            );
            delete place;
         }
         else if constexpr (MANAGED) {
            LglsAssumeDev((*entry)->GetUses() >= 1);
            if ((*entry)->GetUses() == 1) {
               if constexpr (requires { place->~INNER(); })
                  place->~INNER();
               Allocator::Deallocate(DecvqAllCast(*entry));
            }
            else DecvqAllCast(*entry)->AddRef(-1);
         }
      }
      else if (place) {
         NestedDestructor(*place, entry + 1);

         if (not *entry)
            delete place;
         else if constexpr (MANAGED) {
            LglsAssumeDev((*entry)->GetUses() >= 1);
            if ((*entry)->GetUses() == 1)
               Allocator::Deallocate(DecvqAllCast(*entry));
            else
               DecvqAllCast(*entry)->AddRef(-1);
         }
      }
   }

public:
   template<class...A>
   ScopedElement(A&&...arguments) {
      NestedConstructor(element, entries, LglsFwd(arguments)...);
   }
   
   ~ScopedElement() {
      NestedDestructor(element, entries);
   }

   auto operator *  ()       -> T&       {return *element;}
   auto operator *  () const -> T const& {return *element;}
   auto operator -> ()       -> T*       {return  element;}
   auto operator -> () const -> T const* {return  element;}
};
