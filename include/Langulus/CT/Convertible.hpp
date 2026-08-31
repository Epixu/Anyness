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
      template<class Key, int Index>
      struct MorphismReader {
         //friend consteval Types<> MorphismGet(MorphismReader<Key, Index>);
      };

      /// Friend injection coupled with a compile-time counter.               
      /// Basically implements a compile-time vector, that is expanded with   
      /// each specialization of ConverterFrom.                               
      template<class Key, CT::Typelist Values, int Index = unique_id<Key>()>
      struct MorphismWriter {
         static_assert(not Values::Empty,
            "Declaring empty converters is forbidden");

         friend consteval Values MorphismGet(MorphismReader<Key, Index>);
      };

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
            
         if constexpr (requires { MorphismGet(MorphismReader<T, PROGRESS>{}); } ) {
            constexpr decltype(MorphismGet(MorphismReader<T, PROGRESS>{})) to;
            if constexpr (to) {
               ForEach(to, []<class TO> {
                  static_assert(NotConvoluted<TO>, "Strip qualifiers first");
                  static_assert(NotReference<TO>,  "Strip references first");
                  static_assert(NotSheddable<TO>,  "Strip sheddables first");
                  static_assert(not Types<PREV...>::template Contains<TO>,
                     "Morphism redefinition"
                  );
               });
               return GetMorphismsFromInner<T, PROGRESS + 1>(prev + to);
            }
            else return prev;
         }
         else return prev;
      }

      /// Find the ConverterFrom declaration that utilizes FROM -> TO         
      template<class FROM, class TO, int PROGRESS>
      consteval auto FindMorphism() {
         static_assert(NotConvoluted<FROM, TO>, "Strip qualifiers first");
         static_assert(NotReference<FROM, TO>,  "Strip references first");
         static_assert(NotSheddable<FROM, TO>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<FROM>, FROM> and Exact<DecvqAll<TO>, TO>,
            "Strip all decorations on all indirections first");

         if constexpr (requires { MorphismGet(MorphismReader<FROM, PROGRESS>{}); } ) {
            constexpr decltype(MorphismGet(MorphismReader<FROM, PROGRESS>{})) to;
            if constexpr (to) {
               if constexpr (to.template Contains<TO>)
                  return to;
               else
                  return FindMorphism<FROM, TO, PROGRESS + 1>();
            }
            else return Types<>{};
         }
         else return Types<>{};
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
         (not Inner::FindMorphism<DecvqAll<ShedDeref<FROM>>, DecvqAll<ShedDeref<TO>>, 0>().Empty)
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

namespace Langulus::CTTI
{
   /// Defines morphism(s). Each time you specialize                          
   /// ConverterFrom<X, Types<Y...>>, you can define converters from          
   /// multiple places in undefined order. These will be collected the first  
   /// time you reflect a type (once per boundary). Make sure you include all 
   /// relevant converters before relevant type is used.                      
   template<class FROM, CT::Typelist TO>
   struct ConverterFrom;
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
               /// This works even if no `CTTI::MapsTo` or `CTTI::MapsFrom` are defined,  //TODO remove this?
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

      if constexpr (CT::ConvertibleCustom<DFROM, DTO>) {
         using DECL = CTTI::ConverterFrom<DFROM, decltype(CT::Inner::FindMorphism<DFROM, DTO, 0>())>;
         
         if constexpr (requires { {DECL::template Convert<DTO>(from)} -> ::std::same_as<DTO>; })
            return DECL::template Convert<DTO>(from);
         else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
            return DTO(from);
         else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
            return DTO{static_cast<DTO>(DecvqAllCast(from))};
         else {
            static_assert(false,
               "Despite a converter being declared, FROM can't be converted to TO - "
               "add a DECL::template Convert<DTO>, "
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

#define LANGULUS_MORPHISM(FROM, TO) static constexpr \
   ::Langulus::CT::Inner::MorphismWriter<FROM, TO> friend_injection = {}