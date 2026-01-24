///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::MapsTo<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_MapsTo = <type or Types<...>>;` in T       
   template<class T>
   struct MapsTo;
   
   /// Custom converter that can be defined from outside types.               
   /// Used as an alternative to custom constructors and cast operators.      
   template<class FROM, class TO>
   struct Converter;
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected morphisms                      
      template<class T>
      consteval auto GetMorphisms() {
         static_assert(not ::std::is_reference_v<T>,
            "Strip references first");

         if constexpr (CT::Complete<CTTI::MapsTo<T>>) {
            // Checked externally, T doesn't have to be complete        
            using LIST = typename CTTI::MapsTo<T>::Type;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else if constexpr (requires { typename T::CTTI_MapsTo; }) {
            // Checked internally, T has to be a complete type          
            using LIST = typename T::CTTI_MapsTo;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else return NoTypes {};
      };
   }

   /// Check if 'FROM' is convertible to all 'TO'                             
   template<class FROM, class...TO>
   concept Convertible = PartialValidate<TO...>
       and ((::std::convertible_to<FROM, TO>
          or CT::Complete<CTTI::Converter<FROM, TO>>) and ...);

   /// Check if 'FROM' is convertible to one of 'TO'                          
   template<class FROM, class...TO>
   concept ConvertibleToOneOf = PartialValidate<TO...>
       and ((::std::convertible_to<FROM, TO>
          or CT::Complete<CTTI::Converter<FROM, TO>>) or ...);
}

namespace Langulus
{
   /// Get the reflected morphisms, CT::Void if none                          
   template<class T>
   using MorphismsOf = decltype(CT::Inner::GetMorphisms<Decvq<Deref<T>>>());

   /// Convert from one type to another, utilizing CTTI definitions.          
   /// This can work even if no CTTI::MapsTo is defined.                      
   ///   @attention assumes 'from' is constructed                             
   ///   @attention assumes 'to' is NOT constructed                           
   template<class FROM, class TO>
   constexpr void Convert(FROM& from, TO& to) {
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      if constexpr (CT::Complete<CTTI::Converter<DFROM, DTO>>)
         CTTI::Converter<DFROM, DTO>::Convert(from, to);
      else if constexpr (requires { DTO(from); })
         new (&to) DTO(from);
      else if constexpr (requires { DTO(static_cast<DTO>(from)); })
         new (&to) DTO(static_cast<DTO>(from));
      else {
         static_assert(false,
            "FROM can't be converted to TO - add CTTI::Converter, "
            "explicit/implicit constructor, or cast operator"
         );
      }
   }
   
   /// Convert from one type to another, utilizing CTTI definitions.          
   /// This can work even if no CTTI::MapsTo is defined.                      
   ///   @attention assumes 'from' is constructed                             
   template<class TO, class FROM>
   constexpr TO Convert(FROM& from) {
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      if constexpr (CT::Complete<CTTI::Converter<DFROM, DTO>>)
         return CTTI::Converter<DFROM, DTO>::Convert(from);
      else if constexpr (requires { DTO(from); })
         return DTO(from);
      else if constexpr (requires { DTO(static_cast<DTO>(from)); })
         return DTO(static_cast<DTO>(from));
      else {
         static_assert(false,
            "FROM can't be converted to TO - add CTTI::Converter, "
            "explicit/implicit constructor, or cast operator"
         );
      }
   }
}
