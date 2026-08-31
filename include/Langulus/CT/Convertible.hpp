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
   /// Defines morphisms. Each type you specialize ConverterFrom<X>, you get  
   /// a different unique_id() as well, so you can define converters from     
   /// multiple places in undefined order. These will be collected the first  
   /// time you reflect a type per boundary. Make sure you include all        
   /// relevant converters before reflecting the relevant type.               
   template<class FROM, class UID = std::integral_constant<int, unique_id<FROM>()>>
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
      template<class T, int PROGRESS, class...PREV>
      constexpr auto GetMorphismsFromInner(Types<PREV...>&& prev) {
         static_assert(NotConvoluted<T>, "Strip qualifiers first");
         static_assert(NotReference<T>,  "Strip references first");
         static_assert(NotSheddable<T>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<T>, T>,
            "Strip all decorations on all indirections first");
         static_assert((Exact<DecvqAll<PREV>, PREV> and ...),
            "Strip all decorations on all indirections first");
            
         using C = CTTI::ConverterFrom<T, std::integral_constant<int, PROGRESS>>;
         if constexpr (CT::Complete<C>) {
            using Inner = typename C::To;
            if constexpr (CT::Typelist<Inner>) {
               constexpr Inner N = {};
               ForEach(N, []<class TO> {
                  static_assert(NotConvoluted<TO>, "Strip qualifiers first");
                  static_assert(NotReference<TO>,  "Strip references first");
                  static_assert(NotSheddable<TO>,  "Strip sheddables first");
                  static_assert(not Types<PREV...>::template Contains<TO>,
                     "Converter redefinition"
                  );
               });
               return GetMorphismsFromInner<T, PROGRESS + 1>(prev + N);
            }
            else {
               static_assert(NotConvoluted<Inner>, "Strip qualifiers first");
               static_assert(NotReference<Inner>,  "Strip references first");
               static_assert(NotSheddable<Inner>,  "Strip sheddables first");
               static_assert(not Types<PREV...>::template Contains<Inner>,
                  "Converter redefinition"
               );
               return GetMorphismsFromInner<T, PROGRESS + 1>(prev + Types<Inner>{});
            }
         }
         else return prev;
      }

      template<class FROM, class TO, int PROGRESS>
      consteval auto FindMorphism() {
         static_assert(NotConvoluted<FROM, TO>, "Strip qualifiers first");
         static_assert(NotReference<FROM, TO>,  "Strip references first");
         static_assert(NotSheddable<FROM, TO>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<FROM>, FROM> and Exact<DecvqAll<TO>, TO>,
            "Strip all decorations on all indirections first");

         using C = CTTI::ConverterFrom<FROM, std::integral_constant<int, PROGRESS>>;
         if constexpr (CT::Complete<C>) {
            using Inner = typename C::To;
            if constexpr (not CT::Complete<Inner>) {
               if constexpr (Same<TO, Inner>) {
                  static_assert(
                     requires(FROM const& f) { {C::template Convert<TO>(f)} -> ::std::same_as<TO>; },
                     "Converter declared, but lacking implementation of Convert function"
                  );
                  return C {};
               }
               else return FindMorphism<FROM, TO, PROGRESS + 1>();
            }
            else if constexpr (CT::Typelist<Inner>) {
               if constexpr (Inner::template Contains<TO>) {
                  static_assert(
                     requires(FROM const& f) { {C::template Convert<TO>(f)} -> ::std::same_as<TO>; }
                     or ConvertibleImplicit<FROM, TO>
                     or ConvertibleExplicit<FROM, TO>,
                     "Converter declared, but lacking implementation of Convert function"
                  );
                  return C {};
               }
               else return FindMorphism<FROM, TO, PROGRESS + 1>();
            }
            else {
               if constexpr (Same<TO, Inner>) {
                  static_assert(
                     requires(FROM const& f) { {C::template Convert<TO>(f)} -> ::std::same_as<TO>; }
                     or ConvertibleImplicit<FROM, TO>
                     or ConvertibleExplicit<FROM, TO>,
                     "Converter declared, but lacking implementation of Convert function"
                  );
                  return C {};
               }
               else return FindMorphism<FROM, TO, PROGRESS + 1>();
            }
         }
      }

      /// Helper function to extract reflected morphisms from T to other types
      template<class T>
      consteval auto GetMorphismsFrom() {
         return GetMorphismsFromInner<T, 0>(Types<>{});
      };
   }

   /// Check if 'FROM' is custom-convertible to all 'TO'                      
   template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         CT::NotVoid<decltype(Inner::FindMorphism<DecvqAll<ShedDeref<FROM>>,
                                                  DecvqAll<ShedDeref<TO>>, 0>())>
      and ...);

   /*template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         CT::Complete<CTTI::Converter<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>>>
      and ...);*/

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
   //template<class T>
   //using MorphismsTo = decltype(CT::Inner::GetMorphismsTo<Decvq<Deref<T>>>());
      
   /// Get the reflected morphisms from T to other types, CT::Void if none    
   template<class T>
   using MorphismsFrom = decltype(CT::Inner::GetMorphismsFrom<DecvqAll<Deref<T>>>());
      
   /// Convert from one type to another, utilizing CTTI definitions.          
   /// This works even if no `CTTI::MapsTo` or `CTTI::MapsFrom` are defined,  
   /// as long as the types involved have the required constructors and casts.
   ///   @attention assumes 'from' is constructed                             
   ///   @attention does not utilize serialization directly, as this is the   
   ///      lower level conversion routine. It's the other way around:        
   ///      serialization uses this one as a fallback.                        
   template<class TO, class FROM>
   constexpr auto Convert(FROM const& from) -> TO {
      static_assert(CT::NotReference<TO, FROM>, "Strip references first");
      static_assert(CT::NotSheddable<TO, FROM>, "Strip sheddables first");
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      //if constexpr (CT::ConvertibleCustom<DFROM, DTO>)
      //   return CTTI::Converter<DFROM, DTO>::Convert(from);
      if constexpr (CT::ConvertibleCustom<DFROM, DTO>) {
         using M = decltype(CT::Inner::FindMorphism<DFROM, DTO, 0>());
         if constexpr (requires { M::template Convert<DTO>(from); })
            return M::template Convert<DTO>(from);
         else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
            return DTO(from);
         else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
            return DTO{static_cast<DTO>(DecvqAllCast(from))};
         else {
            static_assert(false,
               "Despite a converter being declared, FROM can't be converted to TO - "
               "add a Converter function to the relevant CTTI::ConverterFrom<FROM>, "
               "implicit constructor in TO, or implicit/explicit cast operator in FROM"
            );
         }
      }
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
