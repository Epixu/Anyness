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


/// Useful for creating instances of types on the heap, with multiple levels  
/// of indirection                                                            
template<class T>
struct ScopedElement {
   using CTTI_ReflectAs = void;
   
private:
   T* element = nullptr;

   template<class INNER, class...A>
   static void NestedConstructor(INNER*& place, A&&...arguments) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         if constexpr (requires { new INNER {FWD(arguments)...}; })
            place = new INNER {FWD(arguments)...};
         else if constexpr (requires { new INNER {INNER::FromNumber(FWD(arguments)...)}; })
            place = new INNER {INNER::FromNumber(FWD(arguments)...)};
         else
            static_assert(false, "Unable to construct");
      }
      else {
         place = new INNER {nullptr};
         NestedConstructor(*place, FWD(arguments)...);
      }
   }
   
   template<class INNER>
   static void NestedDestructor(INNER* place) {
      using namespace Langulus;
      if constexpr (CT::Dense<INNER>) {
         #if not LANGULUS_FEATURE(NEWDELETE)
            if constexpr (CT::Referenced<INNER>)
               place->Reference(-1);
         #endif
         delete place;
      }
      else if (place) {
         NestedDestructor(*place);
         delete place;
      }   
   }

public:
   template<class...A>
   ScopedElement(A&&...arguments) {
      NestedConstructor(element, FWD(arguments)...);
   }
   
   ~ScopedElement() {
      NestedDestructor(element);
   }

   auto operator *  ()       -> T&       {return *element;}
   auto operator *  () const -> T const& {return *element;}
   auto operator -> ()       -> T*       {return  element;}
   auto operator -> () const -> T const* {return  element;}
};
