///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Akin.hpp"
#include "../Typenav.hpp"
#include "../Utils/StaticCounter.hpp"
#include "Langulus/Utils/Types.hpp"


namespace Langulus::CTTI
{
   /// Defines morphism(s). Each time you specialize ConverterFrom<X>, you can
   /// define converters from multiple places in undefined order. These will  
   /// be collected the first time you reflect a type (once per boundary).    
   /// Make sure you include all relevant converters before type is used.     
   template<class FROM, class UNIQUE = LglsCounter(FROM)>
   struct ConverterFrom;
}

namespace Langulus::CT
{
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

   namespace Inner
   {
      /// Gather all defined morphism from type T                             
      ///   @return a type list containing all declared conversions           
      template<class T, int PROGRESS, class...PREV>
      constexpr auto GetMorphismsFromInner(Types<PREV...>&& prev) {
         static_assert(NotConvoluted<T>, "Strip qualifiers first");
         static_assert(NotReference<T>,  "Strip references first");
         static_assert(NotSheddable<T>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<T>, T>,
            "Strip all decorations on all indirections first");
         static_assert((Exact<DecvqAll<PREV>, PREV> and ...),
            "Strip all decorations on all indirections first");
         
         using I = std::integral_constant<int, PROGRESS>;
         using M = CTTI::ConverterFrom<T, I>;
         if constexpr (Complete<M>/* requires { CTTI::ConverterFrom<FROM, I>{}; }*/) {
            constexpr typename M::To to;
            if constexpr (to) {
               /*if constexpr (prev.template Contains<typename M::To::First>)
                  return prev;
               else {*/
                  static_assert(not prev.template Contains<typename M::To::First>, 
                     "Unfortunately, partial specialization of ConverterFrom using "
                     "concepts is not allowed, because it doesn't play well with unique_id"
                  );
   
                  ForEach(to, []<class TO> {
                     static_assert(NotConvoluted<TO>, "Strip qualifiers first");
                     static_assert(NotReference<TO>,  "Strip references first");
                     static_assert(NotSheddable<TO>,  "Strip sheddables first");
                     static_assert(not Types<PREV...>::template Contains<TO>,
                        "Morphism redefinition"
                     );
                  });
                  return GetMorphismsFromInner<T, PROGRESS + 1>(prev + to);
               //}
            }
            else return prev;
         }
         else return prev;
      }

      /// Find the ConverterFrom declaration that utilizes FROM -> TO         
      template<class FROM, class TO, int PROGRESS, class PREV = Types<>>
      consteval int FindMorphism() {
         static_assert(NotConvoluted<FROM, TO>, "Strip qualifiers first");
         static_assert(NotReference<FROM, TO>,  "Strip references first");
         static_assert(NotSheddable<FROM, TO>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<FROM>, FROM> and Exact<DecvqAll<TO>, TO>,
            "Strip all decorations on all indirections first");

         using I = std::integral_constant<int, PROGRESS>;
         using M = CTTI::ConverterFrom<FROM, I>;
         if constexpr (Complete<M>/* requires { CTTI::ConverterFrom<FROM, I>{}; }*/) {
            constexpr typename M::To to;
            if constexpr (to) {
               static_assert(not ::std::same_as<typename M::To, PREV>, 
                  "Unfortunately, partial specialization of ConverterFrom using "
                  "concepts is not allowed, because it doesn't play well with unique_id"
               );

               /*if constexpr (::std::same_as<typename M::To, PREV>)
                  return -1; // Converters defined using concepts produce duplicates, and that's a sign we've reached the end
               else {*/
                  if constexpr (to.template Contains<TO>)
                     return PROGRESS;
                  else
                     return FindMorphism<FROM, TO, PROGRESS + 1, typename M::To>();
               //}
            }
            else return -1;
         }
         else return -1;
      }

      /// Helper function to extract reflected morphisms from T to other types
      template<class T>
      consteval auto GetMorphismsFrom() {
         return GetMorphismsFromInner<T, 0>(Types<>{});
      };
   }

   /// Check if 'FROM' is convertible to all 'TO', as per the Langulus        
   /// specification                                                          
   ///   @attention only ConvertibleCustom morphisms are reflected!           
   template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         (Inner::FindMorphism<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>, 0>() >= 0)
      and ...);

   /*template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         CT::Complete<CTTI::Converter<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>>>
      and ...);*/

   /// Check if 'FROM' is convertible to all 'TO', as per C++ specification   
   ///   @attention this is also true if TO are aggregates containing FROM    
   template<class FROM, class...TO>
   concept Convertible = PartialValidate<TO...>
       and ((ConvertibleImplicit<FROM, TO>
          or ConvertibleExplicit<FROM, TO>
          or ConvertibleCustom  <FROM, TO>
       ) and ...);

   /// Check if 'FROM' is somehow convertible to one of 'TO', as per C++      
   /// specification                                                          
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
   //template<class T>
   //using MorphismsTo = decltype(CT::Inner::GetMorphismsTo<Decvq<Deref<T>>>());
      
   /// Get the reflected morphisms from T to other types, CT::Void if none    
   template<class T>
   using MorphismsFrom = decltype(CT::Inner::GetMorphismsFrom<DecvqAll<Deref<T>>>());
      
   /// Convert from one type to another, utilizing CTTI definitions.          
   ///   @attention assumes 'from' is constructed                             
   ///   @attention there is a major difference between conversion and        
   ///      serialization. For example, you can't convert Text -> Text, as    
   ///      the same type is never converter to itself. However, you can      
   ///      serialize Text ~> Text, which will wrap the contents in quotes,   
   ///      and produce a completely different string.                        
   ///   @attention serialization uses conversion routines internally as      
   ///      fallback, but these can be overriden with serialization rules.    
   template<class TO, class FROM>
   constexpr auto Convert(FROM const& from) -> TO {
      static_assert(CT::NotReference<TO, FROM>, "Strip references first");
      static_assert(CT::NotSheddable<TO, FROM>, "Strip sheddables first");
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      if constexpr (CT::ConvertibleCustom<DFROM, DTO>) {
         using I = std::integral_constant<int, CT::Inner::FindMorphism<DFROM, DTO, 0>()>;
         using M = CTTI::ConverterFrom<DFROM, I>;
         
         if constexpr (requires { {M::template Convert<DTO>(from)} -> ::std::same_as<DTO>; })
            return M::template Convert<DTO>(from);
         else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
            return DTO(from);
         else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
            return DTO{static_cast<DTO>(DecvqAllCast(from))};
         else {
            static_assert(false,
               "Despite a converter being declared, FROM can't be converted to TO - "
               "add a M::template Convert<DTO>, "
               "implicit constructor in TO, or implicit/explicit cast operator in FROM"
            );
         }
      }
      /*else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
         return DTO(from);
      else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
         return DTO{static_cast<DTO>(DecvqAllCast(from))};*/
      else {
         static_assert(false,
            "FROM can't be converted to TO - add CTTI::ConverterFrom, "
            "implicit constructor, or implicit/explicit cast operator"
         );
      }
   }
}

#define LANGULUS_MORPHISM(...) using To = Types<__VA_ARGS__>; static_assert(not To::Empty, "Empty morphisms not allowed")