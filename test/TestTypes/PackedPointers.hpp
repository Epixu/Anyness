///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <Langulus/Allocator.hpp>
#include <Langulus/CT/Referenced.hpp>
#include "ReferencedType.hpp"

#if LANGULUS_FEATURE(MANAGED_MEMORY)

/// Packed pointers                                                           
using pptr8  = Langulus::Fractalloc::PackedPointer<char, 2, 6, 0>;
static_assert(sizeof(pptr8) == 1);

using pptr16 = Langulus::Fractalloc::PackedPointer<char, 4, 4, 8>;
static_assert(sizeof(pptr16) == 2);

using pptr32 = Langulus::Fractalloc::PackedPointer<char>;
static_assert(sizeof(pptr32) == 4);


/// Useful for creating instances of types on the heap, with multiple levels  
/// of indirection                                                            
template<::Langulus::CT::CustomPointer T>
struct ScopedElementPacked {
   using CTTI_ReflectAs = void;
   using CTTI_Typed     = T;
   using Inner          = typename T::Type;
   using Allocation     = Langulus::Allocation;
   using AllocationPtr  = Langulus::AllocationPtr;
   using Allocator      = Langulus::Allocator;
   using pot_t          = Langulus::pot_t;
   static constexpr bool Managed = true;

   T* element = nullptr;
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

         if constexpr (Same<NEXT_T, char> and IndirectsOf<T> > 0) {
            ::std::string converted = ::std::to_string(LglsFwd(arguments)...);
            *entry = Allocator::AllocatePacked<INNER>(
               Langulus::MetaDataOf<NEXT_T>(), pot_t(Langulus::Roof2(converted.size()+1)));
            place = (*entry)->GetBlockStartPackedAs<INNER>();

            memcpy(place.Unpack(), converted.c_str(), converted.size()+1);
         }
         else {
            *entry = Allocator::AllocatePacked<INNER>(
               Langulus::MetaDataOf<NEXT_T>(), pot_t(Langulus::Roof2(sizeof(NEXT_T))));
            place = (*entry)->GetBlockStartPackedAs<INNER>();
            
            NestedConstructor(*place, entry + 1, LglsFwd(arguments)...);
         }
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
      *entries = Allocator::Allocate(Langulus::MetaDataOf<T>(), pot_t(Langulus::Roof2(sizeof(T))));
      element = reinterpret_cast<T*>((*entries)->GetBlockStart());

      NestedConstructor(*element, entries + 1, LglsFwd(arguments)...);
   }
   
   ~ScopedElementPacked() {
      NestedDestructor(*element, entries);
   }

   auto operator *  ()       -> T&       {return *element;}
   auto operator *  () const -> T const& {return *element;}
   auto operator -> ()       -> T*       {return  element;}
   auto operator -> () const -> T const* {return  element;}

   /*auto operator *  ()       -> Type&        {return element;}
   auto operator *  () const -> Type const&  {return element;}
   auto operator -> ()       -> Inner*       {return element.Unpack();}
   auto operator -> () const -> Inner const* {return element.Unpack();}*/
};

using pptr8rt  = Langulus::Fractalloc::PackedPointer<RT, 2, 6, 0>;
static_assert(sizeof(pptr8rt) == 1);

using pptr16rt = Langulus::Fractalloc::PackedPointer<RT, 4, 4, 8>;
static_assert(sizeof(pptr16rt) == 2);

using pptr32rt = Langulus::Fractalloc::PackedPointer<RT>;
static_assert(sizeof(pptr32rt) == 4);

#endif