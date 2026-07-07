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
         static_assert(not Convoluted<T>, "Strip qualifiers first");
         static_assert(not Reference<T>, "Strip references first");
         static_assert(not Sheddable<T>, "Strip sheddables first");

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
         static_assert(not Convoluted<T>, "Strip qualifiers first");
         static_assert(not Reference<T>, "Strip references first");
         static_assert(not Sheddable<T>, "Strip sheddables first");

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

   /// Check if 'FROM' is implicitly convertible to all 'TO'                  
   template<class FROM, class...TO>
   concept ConvertibleImplicit = PartialValidate<TO...> and (
         std::is_convertible_v<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>>
      and ...);

   /// Check if 'FROM' is explicitly convertible to all 'TO'                  
   ///   @attention this is also true if TO is aggregate containing FROM      
   template<class FROM, class...TO>
   concept ConvertibleExplicit = PartialValidate<TO...> and (
         std::constructible_from<DecvqAll<ShedDeref<TO>>, DecvqAll<ShedDeref<FROM>>>
      and ...);

   /// Check if 'FROM' is custom-convertible to all 'TO'                      
   template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         CT::Complete<CTTI::Converter<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>>>
      and ...);

   /// Check if 'FROM' is somehow convertible to all 'TO'                     
   ///   @attention this is also true if TO are aggregates containing FROM    
   template<class FROM, class...TO>
   concept Convertible = PartialValidate<TO...>
       and ((ConvertibleImplicit<FROM, TO>
          or ConvertibleExplicit<FROM, TO>
          or ConvertibleCustom  <FROM, TO>
       ) and ...);

   /// Check if 'FROM' is somehow convertible to one of 'TO'                  
   ///   @attention this is also true if one of TO is aggregate of FROM       
   template<class FROM, class...TO>
   concept ConvertibleToOneOf = PartialValidate<TO...>
       and ((ConvertibleImplicit<FROM, TO>
          or ConvertibleExplicit<FROM, TO>
          or ConvertibleCustom  <FROM, TO>
       ) or ...);
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
      static_assert(CT::NotReference<TO, FROM>, "Strip references first");
      static_assert(CT::NotSheddable<TO, FROM>, "Strip sheddables first");
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      if constexpr (CT::ConvertibleCustom<DFROM, DTO>)
         return CTTI::Converter<DFROM, DTO>::Convert(from);
      else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
         return DTO(from);
      else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
         return DTO{static_cast<DTO>(DecvqAllCast(from))};
      else {
         static_assert(false,
            "FROM can't be converted to TO - add CTTI::Converter, "
            "implicit constructor, or implicit/explicit cast operator"
         );
      }
   }
}
