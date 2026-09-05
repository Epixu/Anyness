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
#include "../Utils/Types.hpp"
#include <type_traits>


namespace Langulus::CTTI
{
   /// Defines morphism(s). Each time you specialize Morphism<X>, you can     
   /// define converters from multiple places in undefined order. These will  
   /// be collected the first time you reflect a type (once per boundary).    
   /// Make sure you include all relevant converters before type is used.     
   template<class FROM, int UNIQUE>
   struct Morphism;

   namespace Inner
   {
      template<class T>
      struct MorphismSet;
   }
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
      /// Gather all defined CTTI::Morphism(s) from type T                    
      ///   @return a type list containing all declared conversions           
      template<class T, int PROGRESS = 0, class...PREV>
      constexpr auto GetMorphismsFrom(Types<PREV...>&& prev) {
         static_assert(NotConvoluted<T>, "Strip qualifiers first");
         static_assert(NotReference<T>,  "Strip references first");
         static_assert(NotSheddable<T>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<T>, T>,
            "Strip all decorations on all indirections first");
         static_assert((Exact<DecvqAll<PREV>, PREV> and ...),
            "Strip all decorations on all indirections first");
         
         using M = CTTI::Morphism<T, PROGRESS>;
         if constexpr (requires { M{}; }) {
            constexpr typename M::To to;
            ForEach(to, []<class TO> {
               static_assert(NotConvoluted<TO>, "Strip qualifiers first");
               static_assert(NotReference<TO>,  "Strip references first");
               static_assert(NotSheddable<TO>,  "Strip sheddables first");
               static_assert(not Types<PREV...>::template Contains<TO>,
                  "Morphism redefinition"
               );
            });
            return GetMorphismsFrom<T, PROGRESS + 1>(prev + to);
         }
         else return prev;
      }

      /// Find the CTTI::Morphism declaration that utilizes FROM -> TO        
      ///   @attention each call to this function is a uniquely defined one,  
      ///      and the result might change depending on the include-chain at  
      ///      the point of instantiation.                                    
      template<class FROM, class TO, int PROGRESS = 0, auto UNIQUE = []{}>
      consteval int FindMorphism() {
         static_assert(NotConvoluted<FROM, TO>, "Strip qualifiers first");
         static_assert(NotReference<FROM, TO>,  "Strip references first");
         static_assert(NotSheddable<FROM, TO>,  "Strip sheddables first");
         static_assert(Exact<DecvqAll<FROM>, FROM>,
            "Strip all decorations on all indirections first in FROM");
         static_assert(Exact<DecvqAll<TO>, TO>,
            "Strip all decorations on all indirections first in TO");

         using M = CTTI::Morphism<FROM, PROGRESS>;
         if constexpr (requires { M{}; }) {
            constexpr typename M::To to;
            if constexpr (to.template Contains<TO>) {
               // Prioritize concrete specializations over concept ones 
               constexpr int concrete = FindMorphism<FROM, TO, PROGRESS + 1, UNIQUE>();
               if constexpr (concrete == -1)
                  return PROGRESS;
               else
                  return concrete;
            }
            else return FindMorphism<FROM, TO, PROGRESS + 1, UNIQUE>();
         }
         else return -1;
      }
   }

   /// Check if 'FROM' is convertible to all 'TO', as per the Langulus        
   /// specification. Only such morphisms are reflected!                      
   ///   @attention this concept might change at compile-time! This should be 
   ///      detected by the compiler, so don't worry! If it happens, you can  
   ///      fix it by including the same headers everywhere it is used.       
   template<class FROM, class...TO>
   concept ConvertibleCustom = PartialValidate<TO...> and (
         (Inner::FindMorphism<DecvqAll<ShedDeref<FROM>>,
                              DecvqAll<ShedDeref<TO>>, 0>() >= 0
         ) and ...);

   /// Check if 'FROM' is convertible to all 'TO', as per C++ specification   
   ///   @attention this is also true if TO are aggregates _containing_ FROM  
   ///   @attention this concept might change at compile-time! This should be 
   ///      detected by the compiler, so don't worry! If it happens, you can  
   ///      fix it by including the same headers everywhere it is used.       
   template<class FROM, class...TO>
   concept Convertible = PartialValidate<TO...>
       and ((ConvertibleImplicit<FROM, TO>
          or ConvertibleExplicit<FROM, TO>
          or ConvertibleCustom  <FROM, TO>
       ) and ...);

   /// Check if 'FROM' is somehow convertible to one of 'TO', as per C++      
   /// specification                                                          
   ///   @attention this is also true if one of TO is aggregate of FROM       
   ///   @attention this concept might change at compile-time! This should be 
   ///      detected by the compiler, so don't worry! If it happens, you can  
   ///      fix it by including the same headers everywhere it is used.       
   template<class FROM, class...TO>
   concept ConvertibleToOneOf = PartialValidate<TO...>
       and ((ConvertibleImplicit<FROM, TO>
          or ConvertibleExplicit<FROM, TO>
          or ConvertibleCustom  <FROM, TO>
       ) or ...);
}

namespace Langulus
{ 
   /// Get the reflected morphisms from T to other types, CT::Void if none    
   template<class T>
   using GatherMorphismsFrom = decltype(
      CT::Inner::GetMorphismsFrom<DecvqAll<Deref<T>>>(Types<>{})
   );
      
   /// Convert from one type to another, utilizing CTTI definitions.          
   ///   @attention there is a major difference between conversion and        
   ///      serialization. For example, you can't convert Text -> Text, as    
   ///      the same type is never converter to itself. However, you can      
   ///      serialize Text ~> Text, which will wrap the contents in quotes,   
   ///      and produce a completely different string.                        
   ///   @attention serialization uses conversion routines internally as      
   ///      fallback, but these can be overriden with serialization rules.    
   ///      In other words: serialization is an indirection on top of convert 
   template<class TO, class FROM>
   constexpr auto Convert(FROM const& from) -> TO {
      static_assert(CT::NotReference<TO, FROM>, "Strip references first");
      static_assert(CT::NotSheddable<TO, FROM>, "Strip sheddables first");
      using DFROM = DecvqAll<FROM>;
      using DTO   = DecvqAll<TO>;

      constexpr int found = CT::Inner::FindMorphism<DFROM, DTO, 0>();
      static_assert(found != -1,
         "FROM can't be converted to TO - "
         "define CTTI::Morphism<FROM> that converts it"
      );

      using M = CTTI::Morphism<DFROM, found>;
      if constexpr (requires { {M::template Convert<DTO>(from)} -> ::std::same_as<DTO>; })
         return M::template Convert<DTO>(from);
      else if constexpr (CT::ConvertibleImplicit<DFROM, DTO>)
         return DTO(from);
      else if constexpr (CT::ConvertibleExplicit<DFROM, DTO>)
         return DTO{static_cast<DTO>(DecvqAllCast(from))};
      else {
         static_assert(false,
            "Despite the appropriate CTTI::Morphism being defined, "
            "FROM can't be converted to TO - "
            "either define custom CTTI::Morphism<FROM>::Convert<TO>, "
            "an implicit constructor in TO, "
            "or an implicit/explicit cast operator in FROM"
         );
         return {};
      }
   }
}

#include "../Utils/StaticSet.hpp"

#define LANGULUS_MORPHISM_CONCEPT(FROM, ...) \
   namespace Langulus::CTTI { \
      template<FROM T, int UNIQUE> requires (UNIQUE == GetStaticSetIndex<Inner::MorphismSet<T>, HERE()>()) \
      struct Morphism<T, UNIQUE> { \
         static constexpr bool Conceptual = true; \
         using To = Types<__VA_ARGS__>; \
         static_assert(not To::Empty, "Empty morphisms not allowed"); \
         static_assert(Exact<DecvqAll<T>, T>, "Strip all decorations on all indirections first"); \
      }; \
   }

#define LANGULUS_MORPHISM_CONCEPT_CUSTOM(FROM, BODY, ...) \
   namespace Langulus::CTTI { \
      template<FROM T, int UNIQUE> requires (UNIQUE == GetStaticSetIndex<Inner::MorphismSet<T>, HERE()>()) \
      struct Morphism<T, UNIQUE> { \
         static constexpr bool Conceptual = true; \
         using To = Types<__VA_ARGS__>; \
         static_assert(not To::Empty, "Empty morphisms not allowed"); \
         static_assert(Exact<DecvqAll<T>, T>, "Strip all decorations on all indirections first"); \
         template<class TO> \
         static constexpr TO Convert(ConstAll<T&> from) BODY \
      }; \
   }

#define LANGULUS_MORPHISM(FROM, ...) \
   namespace Langulus::CTTI { \
      template<int UNIQUE> requires (UNIQUE == GetStaticSetIndex<Inner::MorphismSet<FROM>, HERE()>()) \
      struct Morphism<FROM, UNIQUE> { \
         using To = Types<__VA_ARGS__>; \
         static_assert(not To::Empty, "Empty morphisms not allowed"); \
         static_assert(Exact<DecvqAll<FROM>, FROM>, "Strip all decorations on all indirections first"); \
      }; \
   }

#define LANGULUS_MORPHISM_CUSTOM(FROM, BODY, ...) \
   namespace Langulus::CTTI { \
      template<int UNIQUE> requires (UNIQUE == GetStaticSetIndex<Inner::MorphismSet<FROM>, HERE()>()) \
      struct Morphism<FROM, UNIQUE> { \
         using To = Types<__VA_ARGS__>; \
         static_assert(not To::Empty, "Empty morphisms not allowed"); \
         static_assert(Exact<DecvqAll<FROM>, FROM>, "Strip all decorations on all indirections first"); \
         template<class TO> \
         static constexpr TO Convert(FROM const& from) BODY \
      }; \
   }