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
   /// Affects MorphismsFrom                                                  
   /// Define with member `using To = <type or Types<...>>;`                  
   template<class T>
   struct MapsFrom;
   
   /// Affects MorphismsTo                                                    
   /// Define with member `using From = <type or Types<...>>;`                
   template<class T>
   struct MapsTo;
   
   /// Custom converter that can be defined from outside types.               
   /// Used as an alternative to custom constructors and cast operators,      
   /// for the rare cases where you don't have control of either type.        
   template<class FROM, class TO>
   struct Converter;
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected morphisms from others to T     
      template<class T>
      consteval auto GetMorphismsTo() {
         static_assert(not ::std::is_reference_v<T>, "Strip references first");
         if constexpr (CT::Complete<CTTI::MapsTo<T>>) {
            // Checked externally, T doesn't have to be complete        
            using LIST = typename CTTI::MapsTo<T>::From;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else if constexpr (requires { typename T::CTTI_MapsFrom; }) {
            // Checked internally, T has to be a complete type          
            using LIST = typename T::CTTI_MapsFrom;
            if constexpr (CT::Typelist<LIST>)
               return LIST {};
            else
               return Types<LIST> {};
         }
         else return NoTypes {};
      };

      /// Helper function to extract reflected morphisms from T to other types
      template<class T>
      consteval auto GetMorphismsFrom() {
         static_assert(not ::std::is_reference_v<T>, "Strip references first");
         if constexpr (CT::Complete<CTTI::MapsFrom<T>>) {
            // Checked externally, T doesn't have to be complete        
            using LIST = typename CTTI::MapsFrom<T>::To;
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
   /// Get the reflected morphisms from other types to T, CT::Void if none    
   template<class T>
   using MorphismsTo = decltype(CT::Inner::GetMorphismsTo<Decvq<Deref<T>>>());
      
   /// Get the reflected morphisms from T to other types, CT::Void if none    
   template<class T>
   using MorphismsFrom = decltype(CT::Inner::GetMorphismsFrom<Decvq<Deref<T>>>());
      
   /// Convert from one type to another, utilizing CTTI definitions.          
   /// This can work even if no CTTI::MapsTo or CTTI::MapsFrom are defined.   
   ///   @attention assumes 'from' is constructed                             
   template<class TO, class FROM>
   constexpr auto Convert(FROM& from) -> TO {
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
