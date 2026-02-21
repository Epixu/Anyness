///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Allocator.hpp>
#include "ReferencedType.hpp"

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif


/// Packed pointers                                                           
using pptr8  = Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>;
static_assert(sizeof(pptr8) == 1);

using pptr16 = Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>;
static_assert(sizeof(pptr16) == 2);

using pptr32 = Langulus::Fractalloc::PackedPointer<char>;
static_assert(sizeof(pptr32) == 4);

using pptr8rt  = Langulus::Fractalloc::PackedPointer<RT, 2, 6, 0>;
static_assert(sizeof(pptr8rt) == 1);

using pptr16rt = Langulus::Fractalloc::PackedPointer<RT, 4, 4, 8>;
static_assert(sizeof(pptr16rt) == 2);

using pptr32rt = Langulus::Fractalloc::PackedPointer<RT>;
static_assert(sizeof(pptr32rt) == 4);


/// Useful for creating instances of types on the heap, with multiple levels  
/// of indirection                                                            
template<::Langulus::CT::CustomPointer T>
struct ScopedElementPacked {
   using CTTI_ReflectAs = void;
   using Type = T;
   using Inner = typename T::Type;
   using Allocation = Langulus::Allocation;
   using AllocationPtr = Langulus::AllocationPtr;
   using Allocator = Langulus::Allocator;
   static constexpr bool Managed = true;

   T element = nullptr;
   AllocationPtr entries[Langulus::IndirectsOf<T> + 1] = {};

protected:
   template<class INNER, class...A>
   static void NestedConstructor(INNER& place, AllocationPtr* entry, A&&...arguments) {
      using namespace Langulus;

      if constexpr (CT::Dense<INNER>) {
         if constexpr (requires { new INNER (LglsFwd(arguments)...); })
            new (&place) INNER (LglsFwd(arguments)...);
         else if constexpr (requires { new INNER (INNER::FromNumber(LglsFwd(arguments)...)); })
            new (&place) INNER (INNER::FromNumber(LglsFwd(arguments)...));
         else
            static_assert(false, "Unable to construct");
      }
      else {
         using NEXT_T = Deptr<INNER>;
         *entry = Allocator::AllocatePacked<INNER>(
            Langulus::MetaDataOf<NEXT_T>(), pot_t(Roof2(sizeof(NEXT_T))));
         place = (*entry)->GetBlockStartPackedAs<INNER>();
         
         NestedConstructor(*place, entry + 1, LglsFwd(arguments)...);
      }
   }
   
   template<class INNER>
   static void NestedDestructor(INNER place, AllocationPtr* entry) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         if constexpr (CT::Referenced<INNER>)
            place.Reference(-1);

         if (*entry) {
            LglsAssumeDev((*entry)->GetUses() >= 1);
            if ((*entry)->GetUses() == 1) {
               if constexpr (requires { place.~INNER(); })
                  place.~INNER();
               Allocator::Deallocate(DecvqAllCast(*entry));
            }
            else DecvqAllCast(*entry)->AddRef(-1);
         }
      }
      else if (place) {
         NestedDestructor(*place, entry + 1);

         if (*entry) {
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
   ScopedElementPacked(A&&...arguments) {
      NestedConstructor(element, entries, LglsFwd(arguments)...);
   }
   
   ~ScopedElementPacked() {
      NestedDestructor(element, entries);
   }

   auto operator *  ()       -> Type&        {return element;}
   auto operator *  () const -> Type const&  {return element;}
   auto operator -> ()       -> Inner*       {return element.Unpack();}
   auto operator -> () const -> Inner const* {return element.Unpack();}
};
